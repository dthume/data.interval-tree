(ns ^{:doc "Finger tree based interval treeset implementation.

Based largely on the original
[paper](http://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf)

Provides an efficient, persistent representation of a set of items sorted by
the interval which they occupy."}
  org.dthume.data.interval-treeset
  (:require (clojure.core [protocols :as core-protocols]
                          [reducers :as r])
            [clojure.data.finger-tree :as ft]
            [clojure.pprint]
            [clojure.set]
            [org.dthume.data.set :as set-p])
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet ILookup
                         IPersistentStack IPersistentCollection Associative
                         Sorted Reversible Indexed Counted IHashEq)))

(defn- no-codox!
  "Alter `v`ars meta data to mark it as disabled for codox processing."
  [v]
  (alter-meta! v assoc :no-doc true))

(defn- seq-equals [a b]
  (boolean
    (when (or (sequential? b) (instance? java.util.List b))
      (loop [a (seq a), b (seq b)]
        (when (= (nil? a) (nil? b))
          (or
            (nil? a)
            (when (= (first a) (first b))
              (recur (next a) (next b)))))))))

(def ^:private notfound (Object.))

(deftype IntervalMeasure [^long length start end])

(no-codox! #'->IntervalMeasure)

(definline ^:private interval-measure
  [l start end]
  `(->IntervalMeasure ~l ~start ~end))

(defn interval
  "Create an interval with range `s` to `e`"
  [s e]
  [s e])

(definline ^:private m-length
  [^IntervalMeasure i]
  `(.length ~i))

(definline ^:private m-start
  [^IntervalMeasure i]
  `(.start ~i))

(definline ^:private m-end
  [^IntervalMeasure i]
  `(.end ~i))

(defmacro ^:private interval-start
  [x]
  `(nth (~'as-interval ~x) 0))

(defprotocol IPersistentIntervalSet
  (covered-range* [this] "Return the covered range (i.e. min start and max
end) of the items in this set")
  (first-overlapping* [this ival] [this ival n-f]
    "Search `this` for the first value which overlaps `ival`, returning `nil`
if none is found in the two arg case, or `n-f` in the three arg case.")
  (select* [this k])
  (select-overlapping* [this ival]
    "Select the subregion of this set which overlaps `ival`, returning a tuple
of the form `[prefix selected suffix]` where `prefix`, `selected` and
`suffix` are all interval treesets. The methods in
`org.dthume.data.finger-tree.selection` may prove useful in working with the
resulting selection."))

(no-codox! #'IPersistentIntervalSet)

(defn- it-at-least
  [compare-point x]
  #(<= 0 (compare-point (m-end ^IntervalMeasure %) x)))

(defn- it-greater
  [compare-point x]
  #(pos? (compare-point (m-start ^IntervalMeasure %) x)))

(declare ^:private with-tree)
(declare ^:private it-union)
(declare ^:private it-intersection)
(declare ^:private it-difference)

(defn- point-comparator
  [compare-point]
  (fn [a as ae b bs be]
    (as-> (compare-point as bs) x
          (if (zero? x) (- (compare-point ae be)) x)
          (if (zero? x) (compare a b) x))))

(definline ^:private split-tree-key>=
  [compare-point tree k]
  `(ft/split-tree
    ~tree
    #(when-some [ms# (m-start ^IntervalMeasure %)]
       (->> ms#
            (~compare-point ~k)
            pos?
            not))))

(defn- split-tree-key-exact>=
  [as-interval comp-v compare-point tree k ks ke]
  (let [[l x r :as sp] (split-tree-key>= compare-point tree ks)
        [xs xe]        (as-interval x)
        cv             (comp-v k ks ke x xs xe)]
    (if (pos? cv)
      (loop [l (conj l x) r r]
        (if (seq r)
          (let [rf      (first r)
                [rs re] (as-interval rf)
                rc      (comp-v k ks ke rf rs re)]
            (if (pos? rc)
              (recur (conj l rf) (rest r))
              [l rf (rest r)]))
          [(pop l) (peek l) (empty tree)]))
      sp)))

(deftype IntervalTreeSet [as-interval compare-point tree mdata]
  Sequential
  Object
    (equals [_ x]
      (boolean
        (if (instance? java.util.Set x)
          (and (= (count x) (count tree))
               (every? #(contains? x %) tree))
          (seq-equals tree x))))
    (hashCode [_] (reduce + (map ft/hashcode tree)))
  IHashEq
    (hasheq [this]
      (ft/hash-unordered this))
  clojure.lang.IObj
    (meta [_] mdata)
    (withMeta [_ mdata]
      (IntervalTreeSet. as-interval compare-point tree mdata))
  clojure.lang.IFn
    (invoke [this k] (get this k))
    (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))
  Seqable
    (seq [this] (when (seq tree) tree))
  IPersistentCollection
    (cons [this value]
      (if (empty? tree)
        (with-tree this (conj tree value))
        (let [vi       (as-interval value)
              [vs ve]  vi
              [l x r]  (split-tree-key>= compare-point tree vs)
              xi       (as-interval x)
              [xs xe]  xi
              comp-v   (point-comparator compare-point)
              compared (comp-v value vs ve x xs xe)]
          (cond
           (and (zero? compared) (= x value)) this ; already in set
           
           (empty? r)
           (if (pos? compared)
             (with-tree this (conj tree value))
             (with-tree this (conj l value x)))

           (neg? compared)
           (with-tree this (ft/ft-concat (conj l value x) r))

           :else
           (loop [l (conj l x) r r]
             (if (seq r)
               (let [rf      (first r)
                     [rs re] (as-interval rf)
                     rc (comp-v value vs ve rf rs re)]
                 (cond
                  (neg? rc)  (with-tree this (ft/ft-concat (conj l value) r))
                  (zero? rc) this
                  :else      (recur (conj l rf) (rest r))))
               (with-tree this (conj l value))))))))
    (empty [this] (with-tree this (empty tree)))
    (equiv [this x] (.equals this x)) ; TBD
  ISeq
    (first [_] (first tree))
    (more [this]
      (with-tree this (rest tree)))
    (next [this]
      (if-let [t (next tree)]
        (with-tree this t)))
  IPersistentStack
    (peek [_] (peek tree))
    (pop [this] (with-tree this (pop tree)))
  Reversible
    (rseq [_] (rseq tree)) ; not 'this' because tree ops can't be reversed
  ft/Measured
    (measured [_] (ft/measured tree))
    (getMeter [_] (ft/getMeter tree)) ; not needed?
  ft/SplitAt
    (ft-split-at [this n notfound]
      (cond
       (< n 0)
       [(empty this) notfound this]
       
       (< n (count this))
       (let [[l x r] (ft/split-tree tree #(> (m-length ^IntervalMeasure %)
                                             n))]
         [(with-tree this l)
                x
                (with-tree this r)])
       
       :else
       [this notfound (empty this)]))
    (ft-split-at [this n]
      (ft/ft-split-at this n nil))
  Counted
    (count [_] (let [^IntervalMeasure im (ft/measured tree)]
                 (.length im)))
  ILookup
    (valAt [_ k notfound]
      (let [ki      (as-interval k)
            [ks ke] ki]
        (if (empty? tree)
          notfound
          (let [[l x r] (split-tree-key>= compare-point tree ks)]
            (if (= x k)
              k
              (or (->> r
                       (take-while #(as-> % x
                                          (interval-start x)
                                          (compare-point ks x)
                                          (zero? x)))
                       (filter #(= k %))
                       first)
                  notfound))))))
    (valAt [this k]
      (.valAt this k nil))
  IPersistentSet
    (disjoin [this k]
      (if (empty? tree)
        this
        (let [ki        (as-interval k)
              [ks ke]   ki
              comp-v    (point-comparator compare-point)
              [l x r]   (split-tree-key-exact>= as-interval comp-v compare-point
                                                tree k ks ke)]
          (if (= x k)
            (with-tree this (ft/ft-concat l r))
            (loop [curr (first r)
                   rem  (rest r)
                   res  (conj l x)]
              (cond
               (= curr k)
               (with-tree this (if (seq rem) (ft/ft-concat res rem) res))
               
               (or (nil? curr) (-> curr interval-start (not= ks)))
               this
               
               :else
               (recur (first rem) (rest rem) (conj l curr))))))))
    (get [this k] (.valAt this k nil))
  Indexed
    (nth [this n notfound]
      (let [tm ^IntervalMeasure (ft/measured tree)]
        (if (< -1 n (m-length tm))
          (nth (ft/split-tree tree #(> (m-length ^IntervalMeasure %) n)) 1)
          notfound)))
    (nth [this n]
      (let [tm ^IntervalMeasure (ft/measured tree)]
        (if (< -1 n (m-length tm))
          (nth (ft/split-tree tree #(> (m-length ^IntervalMeasure %) n)) 1)
          (throw (IndexOutOfBoundsException.)))))
  Sorted
    (comparator [_]
      (reify java.util.Comparator
        (compare [_ x y]
          (compare-point
           (if x (interval-start x) Integer/MIN_VALUE)
           (if y (interval-start y) Integer/MIN_VALUE)))))
    (entryKey [_ x] x)
    (seq [this ascending?] (if ascending? (.seq this) (rseq tree)))
    (seqFrom [this k asc?]
      (let [[ks ke] (as-interval k)
            [l x r] (split-tree-key>= compare-point tree ks)
            x>k?    (and (empty? r)
                         (some-> x interval-start (compare-point ks) pos?))]
        (cond
         asc?    (with-tree this (ft/conjl r x))
         x>k?    (rseq l)
         :else   (rseq (conj l x)))))
  java.util.Set
    (contains [this x] (not= notfound (get this x notfound)))
    (containsAll [this xs] (every? #(contains? this %) xs))
    (isEmpty [_] (empty? tree))
    (iterator [_]
      (let [t (atom tree)]
        (reify java.util.Iterator
          (next [_] (if-let [f (first @t)]
                      (do (swap! t next)
                          f)
                      (throw (java.util.NoSuchElementException.))))
          (hasNext [_] (boolean (first @t))))))
    (size [this] (count this))
    (toArray [this]
      (clojure.lang.RT/seqToArray tree))
    (toArray [this a]
      (clojure.lang.RT/seqToPassedArray tree a))
  java.util.SortedSet
  (last [this]
    (peek this))
  (headSet [this k]
    (let [[ks ke] (as-interval k)
          [l x r] (split-tree-key>= compare-point tree ks)
          x<k?    (and (empty? r)
                       (some-> x interval-start (compare-point ks) neg?))]
      (if x<k?
        (with-tree this (conj l x))
        (with-tree this l))))
  (subSet [this f t]
    (-> this
        (.tailSet t)
        (.headSet f)))
  (tailSet [this e]
    (.seqFrom this e true))

  set-p/SetAlgebra
  (set-union [this rhs]
    (it-union this rhs))
  (set-intersection [lhs rhs]
    (it-intersection lhs rhs))
  (set-difference [lhs rhs]
    (it-difference lhs rhs))
  IPersistentIntervalSet
    (covered-range* [this]
      (if (empty? tree)
        nil 
        (let [start                (-> this first interval-start)
              ^IntervalMeasure em  (-> this ft/measured)
              end                  (m-end em)]
          [start end])))
    (first-overlapping* [this ival]
      (first-overlapping* this ival nil))
    (first-overlapping* [this ival n-f]
      (let [[ik ip] ival
            [l x r] (ft/split-tree tree (it-at-least compare-point ik))
            [xs xe] (as-interval x)]
        (if (>= 0 (compare-point xs ip))
          x
          n-f)))
    (select-overlapping* [this ival]
      (if (seq this)
        (let [comp-v  (point-comparator compare-point)
              [ik ip] ival
              [l x r] (ft/split-tree tree (it-greater compare-point ip))
              [xs xe] (as-interval x)
              [rs rr] (if (neg? (compare-point ip xs))
                        [l (ft/conjl r x)]
                        [(conj l x) r])]
          (if (seq rs)
            (loop [prefix     (empty tree)
                   covered    (empty tree)
                   [l2 x2 r2] (ft/split-tree rs (it-at-least compare-point ik))]
              (let [prefix           (ft/ft-concat prefix l2)
                    [x2s x2e]        (as-interval x2)
                    [prefix covered] (if (neg? (compare-point x2e ik))
                                       [(conj prefix x2) covered]
                                       [prefix (conj covered x2)])]
                (if (empty? r2)
                  [(with-tree this prefix)
                   (with-tree this covered)
                   (with-tree this rr)]
                  (recur prefix covered
                         (ft/split-tree r2 (it-at-least compare-point ik))))))
            [(empty this) (empty this) (with-tree this rr)]))
        [this this this]))
    (select* [this k]
      (let [ki      (as-interval k)
            [ks ke] ki
            start-not= #(as-> % x
                              (interval-start x)
                              (compare-point ks x)
                              (zero? x))]
        (if (empty? tree)
          [this this this]
          (let [t (split-tree-key>= compare-point tree ks)
                [l x r]
                (loop [l (nth t 0) x (nth t 1) r (nth t 2)]
                  (cond
                   (= x k)         [l (conj (empty this) x) r]
                   (empty? r)      [(conj l x) (empty tree) (empty tree)]
                   (start-not= x)  [l (empty tree) (ft/conjl r x)]
                   :else           (recur (conj l x) (first r) (next r))))]
            [(with-tree this l)
             (with-tree this x)
             (with-tree this r)])))))

(no-codox! #'->IntervalTreeSet)

(defmethod print-method IntervalTreeSet [^IntervalTreeSet o, ^java.io.Writer w]
  (print-method (.tree o) w))

(defmethod clojure.pprint/simple-dispatch IntervalTreeSet
  [^IntervalTreeSet o]
  (clojure.pprint/simple-dispatch (.tree o)))

(defn- with-tree
  [^IntervalTreeSet base tree]
  (IntervalTreeSet. (.as-interval base)
                    (.compare-point base)
                    tree
                    (.mdata base)))

  ;; [l x r] = (split-tree it k)
  ;; prefix  = (concat prefix l)
  ;; if l is empty:
  ;;   
  ;; if r is empty:
  ;;   if x is covered
  ;;     [prefix (conj covered x) []]
  ;;   else if x < k
  ;;     [(conj prefix x) covered []]
  ;;   else if x > k
  ;;     [prefix covered [x]]
  ;; else
  ;;   covered = (conj covered x)
  ;;   it = r
  ;;   recur

(defn partition-around
  [it k]
  (case (count it)
    0 [(empty it) (empty it) (empty it)]
    1 ))

(defn it-intersection*
  "Specific intersection for two interval treesets. Public so clients who
_know_ that they have two interval treesets can avoid the dispatch overhead
of `intersection`."
  [^IntervalTreeSet lhs ^IntervalTreeSet rhs]
  (let [as-interval (.as-interval lhs)
        compare-point (.compare-point lhs)
        [^IntervalTreeSet b ^IntervalTreeSet a]
        (if (identical? lhs (max-key #(or (some-> % first interval-start)
                                          Integer/MIN_VALUE)
                                     lhs rhs))
          [lhs rhs] [rhs lhs])]
    (loop [res (empty (.tree a)) a (.tree a) b (.tree b)]
      (if (or (empty? a) (empty? b))
        (with-tree lhs res)
        (let [bf        (first b)
              [bfs bfe] (as-interval bf)
              [l x r]   (split-tree-key>= compare-point a bfs)]
          (if (= x bf)
            (recur (conj res x) (rest b) r)
            (if-let [remaining
                     (loop [p (conj (empty r) x) c r]
                         (when (seq c)
                           (let [cf (first c)]
                             (when (and cf (-> cf
                                               interval-start
                                               (compare-point bfs)
                                               zero?))
                               (if (= cf bf) (ft/ft-concat p (rest c))
                                   (recur (conj p cf) (rest c)))))))]
              (recur (conj res bf) (rest b) remaining)
              (recur res (rest b) (ft/conjl r x)))))))))

(defn it-intersection
  "Specific intersection for an interval treeset with any other set type.
Public so clients who know they have an interval treeset on the `lhs` can
avoid the dispatch overhead of `intersection`."
  [^IntervalTreeSet lhs rhs]
  (if (instance? IntervalTreeSet rhs)
    (it-intersection* lhs rhs)
    (clojure.set/intersection lhs rhs)))

(defn it-difference*
  "Specific difference for two interval treesets. Public so clients who
_know_ that they have two interval treesets can avoid the dispatch overhead
of `difference`."
  [^IntervalTreeSet lhs ^IntervalTreeSet rhs]
  (let [as-interval (.as-interval lhs)
        compare-point (.compare-point lhs)
        comp-v        (point-comparator compare-point)]
    (loop [res (empty (.tree lhs)) a (.tree lhs) b (.tree rhs)]
      (if (or (empty? a) (empty? b))
        (with-tree lhs (ft/ft-concat res a))
        (let [[bf & br] b
              [bfs bfe] (as-interval bf)
              [l x r] (split-tree-key-exact>= as-interval comp-v compare-point
                                              a bf bfs bfe)
              res (ft/ft-concat res l)
              [xs xe] (as-interval x)
              xc      (comp-v bf bfs bfe x xs xe)]
          (cond
           (= x bf)   (recur res r br)
           (empty? r) (if (pos? xc)
                        (recur (conj res x) r br)
                        (recur res (conj r x) br))
           :else
           (if-let [remaining
                    (loop [p (empty r) c r]
                      (when (some? c)
                        (let [cf (first c)]
                          (when (and cf (-> cf
                                            interval-start
                                            (compare-point bfs)
                                            zero?))
                            (if (= cf bf) (ft/ft-concat p (rest c))
                                (recur (conj p cf) (next c)))))))]
             (if (pos? xc)
               (recur (conj res x) remaining br)
               (recur res (ft/conjl remaining x) br))
             (if (pos? xc)
               (recur (conj res x) r br)
               (recur res (ft/conjl r x) br)))))))))

(defn it-difference
  "Specific difference for an interval treeset with any other set type.
Public so clients who know they have an interval treeset on the `lhs` can
avoid the dispatch overhead of `difference`."
  [^IntervalTreeSet lhs rhs]
  (if (instance? IntervalTreeSet rhs)
   (it-difference* lhs rhs)
   (reduce disj lhs rhs)))

(defn- union-result
  [^IntervalTreeSet this as-interval res bs]
  (let [compare-point (.compare-point this)]
    (cond
     (empty? res)        (with-tree this bs)
     (empty? bs)         (with-tree this res)
     :else
     (let [rf         (peek res)
           [rfs rfe]  (as-interval rf)
           =rfs       #(< rfs (m-start ^IntervalMeasure %))
           [bl x br]  (ft/split-tree bs =rfs)
           [rl rx rs] (ft/split-tree res #(= rfs (m-start ^IntervalMeasure %)))
           ^IntervalTreeSet ot (as-> rs t
                                     (ft/conjl t rx)
                                     (with-tree this t)
                                     (into t bl)
                                     (conj t x))
           new-t      (cond-> (if (seq rl)
                                (ft/ft-concat rl (.tree ot))
                                (.tree ot))
                        (seq br) (ft/ft-concat br))]
       (with-tree this new-t)))))

(defn it-union*
  "Specific union for two interval treesets. Public so clients who
_know_ that they have two interval treesets can avoid the dispatch overhead
of `union`."
  [^IntervalTreeSet lhs ^IntervalTreeSet rhs]
  (let [as-interval   (.as-interval lhs)
        compare-point (.compare-point lhs)
        comp-v        (point-comparator compare-point)]
    (loop [res (empty (.tree lhs)) as (.tree lhs) bs (.tree rhs)]
      (cond
       (empty? as)
       (union-result lhs as-interval res bs)
       
       (empty? bs)
       (union-result lhs as-interval res as)
       
       :else
       (let [[bf & br] bs
             [bfs bfe] (as-interval bf)
             [l x r]   (split-tree-key-exact>= as-interval comp-v compare-point
                                               as bf bfs bfe)]
         (if (= x bf)
           (let [[res br r]
                 (loop [res (conj (ft/ft-concat res l) bf) br br r r]
                   (if (and (seq br) (seq r))
                     (let [bf (first br) rf (first r)]
                       (if (= bf rf)
                         (recur (conj res bf) (rest br) (rest r))
                         [res br r]))
                     [res br r]))]
             (recur res br r))
           (let [[xs xe]   (as-interval x)
                 [l r]     (if (pos? (comp-v bf bfs bfe x xs xe))
                             [(conj l x) r]
                             [l (ft/conjl r x)])
                 new-res (conj (ft/ft-concat res l) bf)]
             (recur new-res br r))))))))

(defn it-union
  "Specific union for an interval treeset with any other set type.
Public so clients who know they have an interval treeset on the `lhs` can
avoid the dispatch overhead of `union`."
  [^IntervalTreeSet lhs rhs]
  (if (instance? IntervalTreeSet rhs)
    (it-union* lhs rhs)
    (into lhs rhs)))
   
(defn- combine-interval-meters
  [compare-point]
  (fn [^IntervalMeasure l ^IntervalMeasure r]
    (let [le (m-end l) re (m-end r)]
      (interval-measure
       (+ (.length l) (.length r))
       (or (m-start r) (m-start l))
       (cond
        (nil? le)                     re
        (nil? re)                     le
        (pos? (compare-point le re))  le
        :else                         re)))))

(defrecord ^:private IntervalMeter [as-interval idElem compare-point]
  ft/ObjMeter
  (measure [_ a]
    (let [i (as-interval a)]
      (interval-measure 1 (nth i 0) (nth i 1))))
  (idElem [_] idElem)
  (opfn [_] (combine-interval-meters compare-point)))

(no-codox! #'->IntervalMeter)
(no-codox! #'map->IntervalMeter)

(defn- interval-meter
  [compare-point as-interval idElem]
  (->IntervalMeter as-interval idElem compare-point))

(defn interval-treeset
  "Create an empty interval tree set. Available options:

:as-interval
: Extract an interval value for an item

:compare-point
: Comparator for two points. Defaults to `clojure.core/compare`.

:zero-interval
: Zero value for the intervals used in this set. Defaults to
  `(interval nil Integer/MIN_VALUE)`."
  [& {:as conf}]
  (let [{:keys [as-interval compare-point zero-interval]
         :or {as-interval identity
              compare-point compare
              zero-interval (interval nil Integer/MIN_VALUE)}} conf
        [zs ze]    zero-interval
        i-m        (interval-meter compare-point as-interval
                                   (interval-measure 0 nil ze))
        empty-tree (ft/->EmptyTree i-m)]
    (IntervalTreeSet. as-interval compare-point empty-tree nil)))

(defn covered-range
  "Return the covered range, i.e. minimum start value and maximum end value, of
the intervals of all the items in interval treeset `t`."
  [t]
  (covered-range* t))

(defn first-overlapping
  "Search `this` for the first value which overlaps `ival`, returning `nil`
if none is found in the two arg case, or `n-f` in the three arg case."
  ([this ival]
     (first-overlapping* this ival))
  ([this ival n-f]
     (first-overlapping* this ival n-f)))

(defn select
  "Return a selection whose selected part contains a single item - `k`."
  [this k]
  (select* this k))

(defn select-overlapping
  "Select the subregion of this set which overlaps `ival`, returning a tuple
of the form `[prefix selected suffix]` where `prefix`, `selected` and
`suffix` are all interval treesets. The methods in
`org.dthume.data.finger-tree.selection` may prove useful in working with the
resulting selection."
  [this ival]
  (select-overlapping* this ival))
