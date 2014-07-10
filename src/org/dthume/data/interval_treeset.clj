(ns ^{:doc "Finger tree based interval treeset implementation.

Based largely on the original
[paper](http://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf)

Provides an efficient, persistent representation of a set of items sorted by
the interval which they occupy."}
  org.dthume.data.interval-treeset
  (:require [clj-tuple :refer (tuple)]
            (clojure.core [protocols :as core-protocols]
                          [reducers :as r])
            [clojure.data.finger-tree :as ft])
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet ILookup
                         IPersistentStack IPersistentCollection Associative
                         Sorted Reversible Indexed Counted IHashEq)))

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

(defrecord IntervalMeasure [^long length ival])

(alter-meta! #'->IntervalMeasure assoc :no-doc true)
(alter-meta! #'map->IntervalMeasure assoc :no-doc true)

(defn- interval-measure
  [l ival]
  (->IntervalMeasure l ival))

(defn interval
  "Create an interval with range `s` to `e`"
  [s e]
  (tuple s e))

(defn- ival-length ^long
  [^IntervalMeasure i]
  (.length i))

(let [ks (tuple :ival 0)]
  (defn- ival-start
    [i]
    (get-in i ks)))

(let [ks (tuple :ival 1)]
  (defn- ival-end
    [i]
    (get-in i ks)))

(defn- split-tree-key
  [compare-point tree k]
  (ft/split-tree tree #(>= 0 (compare-point k (ival-start %)))))

(defprotocol IPersistentIntervalSet
  (union* [this other]
    "Merge `this` set with `other`.")
  (first-overlapping [this ival] [this ival n-f]
    "Search `this` for the first value which overlaps `ival`, returning `nil`
if none is found in the two arg case, or `n-f` in the three arg case.")
  (select [this k])
  (select-overlapping [this ival]
    "Select the subregion of this set which overlaps `ival`, returning a tuple
of the form `[prefix selected suffix]` where `prefix`, `selected` and
`suffix` are all interval treesets. The methods in
`org.dthume.data.finger-tree.selection` may prove useful in working with the
resulting selection."))

(defn- it-at-least
  [compare-point x]
  #(<= 0 (compare-point (ival-end %) x)))

(defn- it-greater
  [compare-point x]
  #(pos? (compare-point (ival-start %) x)))

(declare ^:private with-tree)

(deftype IntervalTreeSet [as-interval compare-point tree mdata]
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
  Seqable
    ; return 'tree' instead of 'this' so that result will be Sequential
    (seq [this] (when (seq tree) tree))
  IPersistentCollection
    (cons [this value]
      (if (empty? tree)
        (with-tree this (conj tree value))
        (let [vi       (as-interval value)
              [vs ve]  vi
              [l x r]  (split-tree-key compare-point tree vs)
              xi       (as-interval x)
              [xs xe]  xi
              compared (compare-point vs xs)
              compared (if (zero? compared)
                         (compare-point ve xe)
                         compared)]
          (if (and (zero? compared) (= x vi))
            this ; already in set
            (let [[a b] (if (>= 0 compared) (tuple value x) (tuple x value))]
              (with-tree this (ft/ft-concat (conj l a) (ft/conjl r b))))))))
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
  ft/ConjL
    (conjl [this a]
      (with-tree this (ft/conjl tree a)))
  ft/Measured
    (measured [_] (ft/measured tree))
    (getMeter [_] (ft/getMeter tree)) ; not needed?
  ft/SplitAt
    (ft-split-at [this n notfound]
      (cond
       (< n 0)
       (tuple (empty this) notfound this)
       
       (< n (count this))
       (let [[l x r] (ft/split-tree tree #(> (ival-length %) n))]
         (tuple (with-tree this l)
                x
                (with-tree this r)))
       
       :else
       (tuple this notfound (empty this))))
    (ft-split-at [this n]
      (ft/ft-split-at this n nil))
  ft/Tree
    (app3 [this ts t2]
      (with-tree this (ft/app3 tree ts t2)))
    (app3deep [this ts t1]
      (with-tree this (ft/app3deep tree ts t1)))
  Counted
    (count [_] (:length (ft/measured tree)))
  ILookup
    (valAt [_ k notfound]
      (let [ki      (as-interval k)
            [ks ke] ki]
        (if (empty? tree)
          notfound
          (let [[l x r] (split-tree-key compare-point tree ks)]
            (if (= x k)
              k
              (or (->> r
                       (take-while #(->> % as-interval first
                                         (compare-point ks)
                                         zero?))
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
              [l x r]   (split-tree-key compare-point tree ks)]
          (if (= x k)
            (with-tree this (ft/ft-concat l r))
            (loop [curr (first r)
                   rem  (next r)
                   res  (conj l x)]
              (cond
               (= curr k)
               (with-tree this (ft/ft-concat res rem))
               
               (or (nil? curr) (-> curr as-interval first (not= ks)))
               this
               
               :else
               (recur (first rem) (next rem) (conj l curr))))))))
    (get [this k] (.valAt this k nil))
  Indexed
    (nth [this n notfound]
      (if (< -1 n (ival-length (ft/measured tree)))
        (nth (ft/split-tree tree #(> (ival-length %) n)) 1)
        notfound))
    (nth [this n]
      (if (< -1 n (ival-length (ft/measured tree)))
        (nth (ft/split-tree tree #(> (ival-length %) n)) 1)
        (throw (IndexOutOfBoundsException.))))
  Sorted
    (comparator [_] compare-point)
    (entryKey [_ x] x)
    (seq [this ascending?] (if ascending? (.seq this) (rseq tree)))
    (seqFrom [this k ascending?]
      (let [[ks ke] (as-interval k)
            [l x r] (split-tree-key compare-point tree ks)]
        (if ascending?
          (with-tree this (ft/conjl r x))
           (rseq (conj l x)))))
  java.util.Set
    (contains [this x] (not= notfound (get this x notfound)))
    (containsAll [this xs] (every? #(contains? this %) xs))
    (isEmpty [_] (empty? tree))
    (iterator [_]
      (let [t (atom tree)]
        (reify java.util.Iterator
          (next [_] (let [f (first @t)]
                      (swap! t next)
                      f))
          (hasNext [_] (boolean (first @t))))))
    (size [this] (count this))
    (toArray [_] nil)
    (toArray [_ a] nil)
  IPersistentIntervalSet
    (^Object union* [^IntervalTreeSet this ^Object other]
      (if-not (instance? IntervalTreeSet other)
        (into this other)
        (let [^IntervalTreeSet iother other]
          (loop [res (empty (.tree this)) as tree bs (.tree iother)]
            (cond
             (empty? as)
             (cond
              (empty? res) (with-tree this bs)
              (empty? bs)  (with-tree this res)
              :else
              (let [rf                     (peek res)
                    [rfs rfe]              (as-interval rf)
                    =rfs                   #(not= rfs (ival-start %))
                    [bl x br]              (ft/split-tree bs =rfs)
                    ^IntervalTreeSet new-t (-> (with-tree this res)
                                               (into bl)
                                               (conj x))
                    new-t                  (-> new-t .tree (ft/ft-concat br))]
                (with-tree this new-t)))

             (empty? bs)
             (cond
              (empty? res) (with-tree this as)
              (empty? as)  (with-tree this res)
              :else
              (let [rf                     (first res)
                    [rfs rfe]              (as-interval rf)
                    =rfs                   #(not= rfs (ival-start %))
                    [bl x br]              (ft/split-tree as =rfs)
                    ^IntervalTreeSet new-t (-> (with-tree this res)
                                               (into bl)
                                               (conj x))
                    new-t                  (-> new-t .tree (ft/ft-concat br))]
                (with-tree this new-t)))
             
             :else
             (let [[bf & br] bs
                   [bfs bfe] (as-interval bf)
                   [l x r]   (split-tree-key compare-point as bfs)
                   [xs xe]   (as-interval x)
                   [l r]    (if (pos? (compare-point bfs xs))
                              (tuple (conj l x) r)
                              (tuple l (ft/conjl r x)))
                   existing? (loop [ls l]
                               (when-let [ll (peek ls)]
                                 (when (->> ll as-interval first
                                            (compare-point bfs) zero?)
                                   (if (= bf ll)
                                     true
                                     (recur (pop ls))))))
                   existing? (if existing? existing?
                                 (loop [ls res]
                                   (when-let [ll (peek ls)]
                                     (when (->> ll as-interval first
                                                (compare-point bfs) zero?)
                                       (if (= bf ll)
                                         true
                                         (recur (pop ls)))))))
                   new-res (ft/ft-concat res l)]
               (if existing?
                 (recur new-res br r)
                 (recur (conj new-res bf) br r))))))))
    (first-overlapping [this ival]
      (first-overlapping this ival nil))
    (first-overlapping [this ival n-f]
      (let [[ik ip] ival
            [l x r] (ft/split-tree tree (it-at-least compare-point ik))]
         (if (>= 0 (compare-point (ival-start x) ip))
           x
           n-f)))
    (select-overlapping [this ival]
      (let [[ik ip] ival
            [l x r] (ft/split-tree tree (it-greater compare-point ip))
            [xs xe] (as-interval x)
            [rs rr] (if (pos? (compare-point ip xs))
                      (tuple (conj l x) r)
                      (tuple l (ft/conjl r x)))]
        (if (empty? rs)
          (tuple (empty this) (empty this) rr)
          (let [[l2 x2 r2] (ft/split-tree rs (it-at-least compare-point ik))]
            (tuple (with-tree this l2)
                   (with-tree this (ft/conjl r2 x2))
                   (with-tree this rr))))))
    (select [this k]
      (let [ki      (as-interval k)
            [ks ke] ki
            start-not= #(->> %
                             as-interval
                             first
                             (compare-point ks)
                             zero?)]
        (if (empty? tree)
          (tuple this this this)
          (let [t (split-tree-key compare-point tree ks)
                [l x r]
                (loop [l (nth t 0) x (nth t 1) r (nth t 2)]
                  (cond
                   (= x k)         (tuple l (conj (empty this) x) r)
                   (empty? r)      (tuple (conj l x) (empty tree) (empty tree))
                   (start-not= x)  (tuple l (empty tree) (ft/conjl r x))             
                   :else           (recur (conj l x) (first r) (next r))))]
            (tuple (with-tree this l)
                   (with-tree this x)
                   (with-tree this r)))))))

(alter-meta! #'->IntervalTreeSet assoc :no-doc true)

(defn- with-tree
  [^IntervalTreeSet base tree]
  (IntervalTreeSet. (.as-interval base)
                    (.compare-point base)
                    tree
                    (.mdata base)))

(defn- combine-interval-meters
  [compare-point]
  (fn [^IntervalMeasure l ^IntervalMeasure r]
    (let [le (ival-end l) re (ival-end r)]
      (interval-measure
       (+ (.length l) (.length r))
       (interval (or (ival-start r) (ival-start l))
                 (cond
                  (nil? le)                     re
                  (nil? re)                     le
                  (pos? (compare-point le re))  le
                  :else                         re))))))

(defrecord ^:private IntervalMeter [as-interval idElem compare-point]
  ft/ObjMeter
  (measure [_ a] (interval-measure 1 (as-interval a)))
  (idElem [_] idElem)
  (opfn [_] (combine-interval-meters compare-point)))

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
              combine-intervals
              (fn [^IntervalMeasure l ^IntervalMeasure r]
                (let [le (ival-end l) re (ival-end r)]
                  (interval-measure
                   (+ (.length l) (.length r))
                   (interval (or (ival-start r) (ival-start l))
                             (cond
                              (nil? le)                     re
                              (nil? re)                     le
                              (pos? (compare-point le re))  le
                              :else                         re)))))
              interval-meter (interval-meter compare-point as-interval
                                             (interval-measure 0 zero-interval))
              empty-tree (ft/->EmptyTree interval-meter)]
    (IntervalTreeSet. as-interval compare-point empty-tree nil)))

(defn union
  "Return a set that is the union of the input sets"
  ([s1]
     s1)
  ([s1 s2]
     (union* s1 s2))
  ([s1 s2 & sets]
     (reduce union* (union* s1 s2) sets)))

(defn intersection
  "Return a set that is the intersection of the input sets"
  ([s1]
     s1)
  ([s1 s2]
     (clojure.set/intersection s1 s2))
  ([s1 s2 & sets]
     (apply clojure.set/intersection s1 s2 sets)))

(defn difference
  "Return a set that is the first set without elements of the remaining sets"
  ([s1]
     s1)
  ([s1 s2]
     (clojure.set/difference s1 s2))
  ([s1 s2 & sets]
     (apply clojure.set/difference s1 s2 sets)))
