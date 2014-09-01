;; ## Intro

;; This walkthrough aims to introduce the
;; [data.finger-tree](https://github.com/dthume/data.interval-tree)
;; library via an annotated code walkthrough. It also functions as the
;; set of unit tests for the library. The fact that this can be done
;; while still producing a fairly pleasant experience from both points
;; of view is quite humbling, frankly. Which is fitting given that the
;; experience of implementing the data structure was similarly humbling.
(ns org.dthume.data.test-interval-treeset
  (:require [midje.sweet :refer :all]
            [org.dthume.data.interval-treeset :as it]
            [org.dthume.data.interval-treeset.selection :as sel]
            [org.dthume.data.set :as set]))

;; ## Utilities

;; A basic transformation function for use in update tests.
;; For brevity the majority of our examples will simply use sets or raw
;; intervals rather than using a custom interval lens, and this gives
;; us an easy to read way of illustrating transformations over one or
;; more of the values contained in a set.
(defn- shiftt
  [op x]
  (fn [[s e]]
    (it/interval (op s x) (op e x))))

;; Utility fn for creating treesets succinctly
(let [empty-set (it/interval-treeset)]
  (defn ts [& items]
    (if (empty? items) empty-set
        (into empty-set items))))

;; Another for treesets using a custom interval lens
;; When we want to store something other than raw intervals, we can supply a
;; lensing function with `:as-interval` which must be a function of one
;; argument which, when applied to a set item, returns an interval.
(let [empty-set (it/interval-treeset :as-interval :span)]
  (defn cts [& items] (into empty-set items)))

;; ## Walkthrough

;; Interval treesets function as efficient sorted sequences:
;;
;; * [`count`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count)
;;   is O(1)
;; * [`first`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/first)
;;   is amortized O(1)
;; * [`conj`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj)
;;   is amortized O(log(n))
(fact "Interval treesets support basic seq operations"
  (= (ts [1 2]) (ts [1 2]))             => true

  (count (ts [1 2] [3 4]))              => 2

  (first (ts [3 4] [1 2]))              => [1 2]

  (second (ts [3 4] [1 2]))             => [3 4]

  (conj (ts [1 2]) [3 4])               => [[1 2] [3 4]])

;; And of course they're sets, so adding duplicate items is a noop.
;; Items are added at the correct location based on the interval and,
;; if a custom lens is used, do not themselves have to be intervals.
(fact "Interval treesets cannot contain duplicate items"
  (conj (ts [1 2]) [1 2])               => [[1 2]]

  (conj (ts [0 3] [4 6] [7 8]) [0 6])   => [[0 6] [0 3] [4 6] [7 8]]

  (conj (cts {:span [0 3] :key :a}
             {:span [4 6] :key :b}
             {:span [7 9] :key :c})
        {:span [0 6] :key :ab})         => [{:span [0 6] :key :ab}
                                            {:span [0 3] :key :a}
                                            {:span [4 6] :key :b}
                                            {:span [7 9] :key :c}])

;; Interval tree sets can be used as efficient indexed collections:
;;
;; * [`nth`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/nth)
;;   is amortized O(log(n))
;; * [`contains?`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/contains?)
;;   is amortized O(log(n))
(fact "Interval treesets support indexed operations"
  (nth (ts [1 2] [3 4] [5 6]) 2)        => [5 6]

  (contains? (ts [1 2] [3 4]) [1 2])    => true

  (contains? (ts [1 2] [3 4]) [1 3])    => false

  (subseq (ts [1 2] [3 4] [5 6] [7 8])
          > [3 4])                      => [[5 6] [7 8]]

  (subseq (ts [1 2] [3 4] [5 6] [7 8])
          < [5 6])                      => [[1 2] [3 4]]

  (subseq (ts [1 2] [3 4] [5 6] [7 8])
          <= [3 4])                     => [[1 2] [3 4]]

  (subseq (ts [1 2] [3 4] [5 6] [7 8])
          >= [5 6])                     => [[5 6] [7 8]])

;; Interval treesets are equally efficient at either end:
;;
;; * [`peek`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/peek)
;;   is amortized O(1)
;; * [`pop`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/pop)
;;   is amortized O(1)
;; * [`rseq`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rseq)
;;   is amortized O(1)
(fact "Interval trees support stack operations"
  (peek (ts [1 2] [3 4] [5 6]))         => [5 6]

  (pop (ts [1 2] [3 4] [5 6]))          => [[1 2] [3 4]]

  (rseq (ts [1 2] [3 4] [5 6]))         => [[5 6] [3 4] [1 2]])

;; Interval treesets function as associative collections:
;;
;; * [`get`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/get)
;;   is amortized O(log(n))
;; * [`disj`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/disj)
;;   is amortized O(log(n))
;;
;; Note that `get`, `disj` etc. expect items, not necessarily
;; intervals, as show by the example with a custom interval lens.
(fact "Interval treesets support associative operations"
  (get (ts [1 2] [3 4]) [3 4])          => [3 4]

  (get (ts [1 2] [3 4]) [5 6])          => nil

  (get (ts [1 2] [3 4]) [5 6] :foo)     => :foo

  (get (cts {:span [0 5]   :key :a}
            {:span [6 10]  :key :b}
            {:span [11 15] :key :c}
            {:span [11 21] :key :d}
            {:span [16 21] :key :e}
            {:span [22 25] :key :f})
       {:span [11 15] :key :c})         => {:span [11 15] :key :c}

  (disj (ts [1 2] [3 4] [5 6]) [3 4])   => [[1 2] [5 6]])

;; Finding the covered range quickly is useful for, among other things,
;; creating [core.logic](https://github.com/clojure/core.logic)
;; finite domains. Now that you're armed with _that_ answer, I'm sure you'll
;; think of an interesting question...
;;
;; * [`covered-range`](../codox/org.dthume.data.interval-treeset.html#var-covered-range)
;;   is amortized O(1).
(fact "Interval treesets can efficiently find the covered range"
  (it/covered-range (ts [1 2] [5 6] [8 9]))
                                        => [1 9]

  (it/covered-range (ts))               => nil)

;; The merge algorithm is based on the one in the
;; [original paper](http://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf),
;; although I confess that the differences between the Haskell in the paper
;; and the clojure finger tree implementation mean I'm not altogether confident
;; that it's completely correct (that said the tests seem favourable thus far).
;; Assuming that it _is_ correct then:
;;
;; * [`union`](../codox/org.dthume.data.interval-treeset.html#var-union)
;;   is:
;;
;;     - O(m log(n/m)) in the general case of merging another interval treeset.
;;     - O(log m) for two treesets where max xs <= min ys (or vice versa).
;;
;; Note that if one of the latter arguments is _not_ an interval treeset then
;; it will simply be merged with
;; [`conj`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj).
(fact "Interval treesets support efficient merging"
  (set/union (ts [1 2] [3 4])
             (ts [5 6] [7 8]))           => [[1 2] [3 4] [5 6] [7 8]]

  (set/union (ts [1 2] [5 6])
             (ts [3 4] [7 8]))           => [[1 2] [3 4] [5 6] [7 8]]

  (set/union (ts [1 2] [3 4])
             (ts [3 4] [7 8]))           => [[1 2] [3 4] [7 8]]

  (set/union (ts [1 2] [7 10] [7 8])
             (ts [3 4] [7 8]))           => [[1 2] [3 4] [7 10] [7 8]]

  (set/union (ts [3 4] [7 8])
             (ts [1 2] [7 10] [7 8]))    => [[1 2] [3 4] [7 10] [7 8]]

  (set/union (ts [1 2] [7 8])
             (ts [1 2] [7 10] [7 8]))    => [[1 2] [7 10] [7 8]]

  (set/union (ts [0 2] [1 2] [7 8])
             (ts [1 2] [7 10] [7 8]))    => [[0 2] [1 2] [7 10] [7 8]])

;; [`intersection`](../codox/org.dthume.data.interval-treeset.html#var-intersection)
;; is currently using a naive implementation (the default
;; from
;; [`clojure.set`](http://clojure.github.io/clojure/clojure.set-api.html)).
(fact "Interval treesets support intersections"
  (set/intersection (ts [1 2] [3 4])
                    (ts [3 4] [5 6]))    => [[3 4]]

  (set/intersection (ts [1 2] [3 4] [5 6] [7 8] [9 10])
                    (ts [3 4] [7 8]))    => [[3 4] [7 8]]
  )

;; [`difference`](../codox/org.dthume.data.interval-treeset.html#var-difference)
;; is also currently using a naive implementation (the default
;; from
;; [`clojure.set`](http://clojure.github.io/clojure/clojure.set-api.html)).
(fact "Interval treesets support differences"
  (set/difference (ts [1 2] [3 4])
                  (ts [3 4] [5 6]))      => [[1 2]]

  (set/difference (ts [1 2] [3 4])
                  (ts [1 2] [5 6]))      => [[3 4]]

  (set/difference (ts [1 2] [3 4])
                  (ts [1 2] [3 4]))      => []

  (set/difference (ts [1 2] [3 4])
                  (ts [5 6] [7 8]))      => [[1 2] [3 4]])

;; Lookup of the first overlapping item, as described in the original paper
;; 
;; * [`first-overlapping`](../codox/org.dthume.data.interval-treeset.html#var-first-overlapping)
;;   is O(log(n)).
(fact "Interval treesets support lookup of overlapping items"
  (it/first-overlapping
   (ts [1 2] [3 4] [5 6] [7 8])
   [3 6])                               => [3 4]

  (it/first-overlapping
   (cts {:span [6 10]}
        {:span [11 15]}
        {:span [11 21]}
        {:span [16 21]}
        {:span [22 25]})
   [11 21])                             => {:span [11 21]})

(fact "Interval treesets are `java.util.Set` instances"
  (seq (.toArray (ts [1 2] [3 4])))     => [[1 2] [3 4]])

;; ## Selections

;; Here we introduce selection regions, which are a partitioning of the
;; set around a particular subset of consecutive items.
;; Selection regions are indexed sequences of 3 items:
;; `[prefix selected suffix]`, which is useful in its own right, but
;; see below for examples of using the
;; [`org.dthume.data.interval-tree.selection`](../codox/org.dthume.data.interval-treeset.selection.html)
;; namespace to work with selection regions.
(fact "Treesets can be partitioned around selection regions"
  (it/select-overlapping
   (ts [1 2] [3 4] [5 6] [7 8])
   [3 6])                               => [[[1 2]]
                                            [[3 4] [5 6]]
                                            [[7 8]]]

  (it/select-overlapping
   (ts [0 10] [1 2] [3 4] [5 6] [7 8])
   [3 6])                               => [[[1 2]]
                                            [[0 10] [3 4] [5 6]]
                                            [[7 8]]])

;; Each component of a selection region acts as a window, and can be operated
;; on individually, but the `selected` component acts as the "primary"
;; component, and can be expanded or contracted to the left and right sides
;; using a variety of functions. by convention, functions which manipulate the
;; left side of the selected region are suffixed with "l", e.g.
;; [`expandl`](../codox/org.dthume.data.interval-treeset.selection.html#var-expandl),
;; [`contractl`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractl),
;; [`contractl-while`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractl-while),
;; while those which manipulate the right
;; side are suffixed with "r", e.g.
;; [`expandr`](../codox/org.dthume.data.interval-treeset.selection.html#var-expandr),
;; [`contractr`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractr),
;; [`contractr-by`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractr-by).
;;
;; Functions are provided for:
;;
;; * Expanding / contracting by a specific number of items
;;   ([`expandl`](../codox/org.dthume.data.interval-treeset.selection.html#var-expandl),
;;    [`contractl`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractl),
;;    [`expandr`](../codox/org.dthume.data.interval-treeset.selection.html#var-expandr),
;;    [`contractr`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractr))
;;
;; * Expanding / contracting until a predicate returns logical `false`
;;   ([`expandl-while`](../codox/org.dthume.data.interval-treeset.selection.html#var-expandl-while),
;;    [`contractl-while`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractl-while),
;;    [`expandr-while`](../codox/org.dthume.data.interval-treeset.selection.html#var-expandr-while),
;;    [`contractr-while`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractr-while))
;;
;; * Expanding / contracting until a predicate has returned logical `true` a
;;   set number of times:
;;   ([`expandl-by`](../codox/org.dthume.data.interval-treeset.selection.html#var-expandl-by),
;;    [`contractl-by`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractl-by),
;;    [`expandr-by`](../codox/org.dthume.data.interval-treeset.selection.html#var-expandr-by),
;;    [`contractr-by`](../codox/org.dthume.data.interval-treeset.selection.html#var-contractr-by))
(fact "The selected subset can be expanded or contracted on either side"
  (-> (ts [1 2] [3 4] [5 6] [7 8])
      (it/select [3 4])
      (sel/selected))                   => [[3 4]]

  (as-> (ts [1 2] [3 4] [5 6] [7 8]) x
        (it/select-overlapping x [0 10])
        (mapv count x))                 => [0 4 0]

  (-> (ts [1 2] [3 4] [5 6])
      (it/select-overlapping [3 4])
      (sel/expandl 1)
      (sel/selected))                   => [[1 2] [3 4]]

  (-> (ts [1 2] [3 4] [5 6])
      (it/select-overlapping [3 4])
      (sel/expandl 1)
      (sel/expandr 1)
      (sel/selected))                   => [[1 2] [3 4] [5 6]]

  (-> (ts [1 2] [3 4] [5 6])
      (it/select-overlapping [0 10])
      (sel/contractl 1)
      (sel/contractr 1)
      (sel/selected))                   => [[3 4]]

  (-> (cts {:span [0 5]   :key :a}
           {:span [6 10]  :key :b}
           {:span [11 15] :key :d}
           {:span [11 21] :key :c}
           {:span [16 21] :key :e}
           {:span [22 25] :key :f})
      (it/select-overlapping [6 10])
      (sel/expandr-while #(not= :e (:key %)))
      (sel/selected))                   => [{:span [6 10] :key :b}
                                            {:span [11 21] :key :c}
                                            {:span [11 15] :key :d}])

;; Components of a selection region can be individually transformed using
;; the
;; [`edit`](../codox/org.dthume.data.interval-treeset.selection.html#var-edit)
;; and
;; [`transform`](../codox/org.dthume.data.interval-treeset.selection.html#var-transform)
;; functions which are similar to
;; [`clojure.core/->`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/-%3E)
;; and
;; [`clojure.core/->>`](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/-%3E%3E)
;; except that they operate on functions:
;; `edit` passes the current value of a component as the _first_ argument to
;; the supplied function, while `transform` passes the current value as the
;; _last_ argument.
;;
;; Both `edit` and `transform` use _lenses_ to select a particular component
;; part to work with; in this case there are three lenses:
;; [`prefix`](../codox/org.dthume.data.interval-treeset.selection.html#var-prefix),
;; [`selected`](../codox/org.dthume.data.interval-treeset.selection.html#var-selected)
;; and
;; [`suffix`](../codox/org.dthume.data.interval-treeset.selection.html#var-suffix),
;; all in the
;; [`org.dthume.data.interval-tree.selection`](../codox/org.dthume.data.interval-treeset.selection.html)
;; namespace.
(facts "The components of a selection can be individually transformed"
  (fact "Edit passes the component as the first arg to f"
   (-> (ts [1 2] [3 4] [5 6] [7 8])
       (it/select-overlapping [5 6])
       (sel/edit sel/prefix set/intersection
                 (ts [1 2] [9 10]))
       (sel/unselect))                   => [[1 2] [5 6] [7 8]])

  (fact "Transform passes the component as the last arg to f"
    (-> (ts [1 2] [3 4] [5 6] [7 8])
        (it/select-overlapping [3 6])
        (sel/transform sel/selected map
                       (shiftt + 1))
        (sel/unselect))                   => [[1 2] [4 5] [6 7] [7 8]]))

;; Both `edit` and `transform` coerce the result of the supplied function to an
;; interval treeset, enabling transformation functions to return `nil`,
;; sequence types or another interval treeset (in which case it must use the
;; same meter as the original, which will be the case if it is created from the
;; original).
;; 
;; For more details see the
;; [codox docs](../codox/org.dthume.data.interval-treeset.selection.html#var-edit).
(fact "Individual parts of selected regions can be discarded"
  (-> (ts [1 2] [3 4] [5 6] [7 8])
      (it/select-overlapping [3 6])
      (sel/edit sel/prefix empty)
      (sel/unselect))                   => [[3 4] [5 6] [7 8]] 

  (-> (ts [1 2] [3 4] [5 6] [7 8])
      (it/select-overlapping [3 6])
      (sel/edit sel/selected empty)
      (sel/unselect))                   => [[1 2] [7 8]]

  (-> (ts [1 2] [3 4] [5 6] [7 8])
      (it/select-overlapping [3 6])
      (sel/edit sel/suffix empty)
      (sel/unselect))                   => [[1 2] [3 4] [5 6]])

;; For even more inline goodness, there are three threading operators provided,
;; [`->`](../codox/org.dthume.data.interval-treeset.selection.html#var--.3E),
;; [`->>`](../codox/org.dthume.data.interval-treeset.selection.html#var--.3E.3E)
;; and
;; [`as->`](../codox/org.dthume.data.interval-treeset.selection.html#var-as-.3E),
;; which mirror those in `clojure.core` except that they
;; take an extra leading argument: a selection region and a lensing function.
;;
;; These functions extract a component part from the selection region using
;; the lens, thread it through the body of expressions, then return an updated
;; selection region with the result of the body in place of the original
;; component part.
;; Results from the body expressions are coerced to treesets as with
;; [`edit`](../codox/org.dthume.data.interval-treeset.selection.html#var-edit).
;;
;; One downside of these functions is that they prevent you from just `refer`ing
;; the entire selection namespace. But you wouldn't do that anyway, right?
(facts "Threading operators can be used to work with selection region components"
  (fact "-> is analagous to clojure.core/->"
    (-> (ts [1 2] [3 4] [5 6] [7 8])
        (it/select-overlapping [5 6])
        (sel/-> sel/prefix
                (set/intersection (ts [1 2]))
                (set/union (ts [0 1])))
        (sel/unselect))                   => [[0 1] [1 2] [5 6] [7 8]])

  (fact "->> is analagous to clojure.core/->>"
    (-> (ts [1 2] [3 4] [5 6] [7 8])
        (it/select-overlapping [3 6])
        (sel/->> sel/selected
                 (map (shiftt + 1))
                 (map (shiftt - 2)))
        (sel/unselect))                   => [[1 2] [2 3] [4 5] [7 8]])

  (fact "as-> is analagous to clojure.core/as->"
    (-> (ts [1 2] [3 4] [5 6] [7 8])
        (it/select-overlapping [3 6])
        (sel/as-> sel/selected x
          (set/intersection x (ts [3 4] [4 5]))
          (map (shiftt + 1) x))
        (sel/unselect))                   => [[1 2] [4 5] [7 8]]))
