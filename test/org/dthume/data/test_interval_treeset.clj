(ns org.dthume.data.test-interval-treeset
  (:require [midje.sweet :refer :all]
            [clj-tuple :refer (tuple)]
            [clojure.data.finger-tree :as ft]
            [org.dthume.data.interval-treeset :as it]
            [org.dthume.data.interval-treeset.selection :as sel]))

;; A basic transformation function for use in update tests
(defn- shiftt
  [op x]
  (fn [[s e]]
    (tuple (op s x) (op e x))))

;; Utility fn for creating treesets succinctly
(let [empty-set (it/interval-treeset)]
  (defn ts [& items] (into empty-set items)))

;; Another for treesets using a custom interval lens
;; When we want to store something other than raw intervals, we can supply a
;; lensing function with `:as-interval` which must be a function of one
;; argument which, when applied to a set item, returns an interval.
(let [empty-set (it/interval-treeset :as-interval :span)]
  (defn cts [& items] (into empty-set items)))

;; Interval treesets function as efficient sorted sequences:
;;
;; *  `count` is O(1)
;; * `first` is amortized O(1)
;; * `conj` is amortized O(log(n))
(fact "Interval trees support basic seq operations"
  (= (ts [1 2]) (ts [1 2]))             => true

  (count (ts [1 2] [3 4]))              => 2

  (first (ts [3 4] [1 2]))              => [1 2]

  (second (ts [3 4] [1 2]))             => [3 4]

  (conj (ts [1 2]) [3 4])               => [[1 2] [3 4]])

;; Interval tree sets can be used as efficient indexed collections:
;;
;; * `nth` is amortized O(log(n)) 
;; * `contains?` is amortized O(log(n))
(fact "Interval trees support indexed operations"
  (nth (ts [1 2] [3 4] [5 6]) 2)        => [5 6]

  (contains? (ts [1 2] [3 4]) [1 2])    => true
  (contains? (ts [1 2] [3 4]) [1 3])    => false)

;; Interval tree sets are equally efficient at either end:
;;
;; * `peek` is amortized O(1)
;; * `pop` is amortized O(1)
(fact "Interval trees support stack operations"
  (peek (ts [1 2] [3 4] [5 6]))         => [5 6]

  (pop (ts [1 2] [3 4] [5 6]))          => [[1 2] [3 4]])

;; Interval tree sets function as associative collections:
;;
;; * `get` is amortized O(log(n))
;; * `disj` is amortized O(log(n))
(fact "Interval trees support associative operations"
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

(fact "Interval trees support efficient merging"
  (it/union (ts [1 2] [3 4])
            (ts [5 6] [7 8]))           => [[1 2] [3 4] [5 6] [7 8]]

  (it/union (ts [1 2] [5 6])
            (ts [3 4] [7 8]))           => [[1 2] [3 4] [5 6] [7 8]]

  (it/union (ts [1 2] [3 4])
            (ts [3 4] [7 8]))           => [[1 2] [3 4] [7 8]]

  (it/union (ts [1 2] [7 10] [7 8])
            (ts [3 4] [7 8]))           => [[1 2] [3 4] [7 8] [7 10]]

  (it/union (ts [3 4] [7 8])
            (ts [1 2] [7 10] [7 8]))    => [[1 2] [3 4] [7 8] [7 10]]

  (it/union (ts [1 2] [7 8])
            (ts [1 2] [7 10] [7 8]))    => [[1 2] [7 8] [7 10]]

  (it/union (ts [0 2] [1 2] [7 8])
            (ts [1 2] [7 10] [7 8]))    => [[0 2] [1 2] [7 8] [7 10]])

(fact "Interval trees support intersections"
  (it/intersection (ts [1 2] [3 4])
                   (ts [3 4] [5 6]))    => [[3 4]])

(fact "Interval trees support differences"
  (it/difference (ts [1 2] [3 4])
                 (ts [3 4] [5 6]))      => [[1 2]])

(fact "Interval trees support selection of overlapping subregions"
  (sel/overlapping-subset
   (ts [1 2] [3 4] [5 6] [7 8])
   [3 6])                               => [[3 4] [5 6]]

  (it/first-overlapping
   (ts [1 2] [3 4] [5 6] [7 8])
   [3 6])                               => [3 4]

  (it/first-overlapping
   (cts {:span [6 10]}
        {:span [11 15]}
        {:span [11 21]}
        {:span [16 21]}
        {:span [22 25]})
   [11 21])                             => {:span [11 15]}

  (it/select-overlapping
   (ts [1 2] [3 4] [5 6] [7 8])
   [3 6])                               => [[[1 2]]
                                            [[3 4] [5 6]]
                                            [[7 8]]])

(fact "Individual parts of selections can be discarded"
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
           {:span [11 15] :key :c}
           {:span [11 21] :key :d}
           {:span [16 21] :key :e}
           {:span [22 25] :key :f})
      (it/select-overlapping [6 10])
      (sel/expandr-while #(not= :d (:key %)))
      (sel/selected))                   => [{:span [6 10] :key :b}
                                            {:span [11 15] :key :c}])

(facts "The components of a selection can be individually transformed"
  (fact "Edit passes the component as the first arg to f"
   (-> (ts [1 2] [3 4] [5 6] [7 8])
       (it/select-overlapping [5 6])
       (sel/edit sel/prefix it/intersection
                 (ts [1 2] [9 10]))
       (sel/unselect))                   => [[1 2] [5 6] [7 8]])

  (fact "Transform passes the component as the last arg to f"
    (-> (ts [1 2] [3 4] [5 6] [7 8])
        (it/select-overlapping [3 6])
        (sel/transform sel/selected map
                       (shiftt + 1))
        (sel/unselect))                   => [[1 2] [4 5] [6 7] [7 8]]))

(facts "The components of a selection can be individually transformed"
  (fact "-> is analagous to clojure.core/->"
    (-> (ts [1 2] [3 4] [5 6] [7 8])
        (it/select-overlapping [5 6])
        (sel/-> sel/prefix
                (it/intersection (ts [1 2]))
                (it/union (ts [0 1])))
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
          (it/intersection x (ts [3 4] [4 5]))
          (map (shiftt + 1) x))
        (sel/unselect))                   => [[1 2] [4 5] [7 8]]))
