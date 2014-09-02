(ns org.dthume.data.test-check-interval-treeset
  (:require [clojure.core.reducers :as r]
            [clojure.test.check :as tc]
            (clojure.test.check [clojure-test :refer [defspec]]
                                [generators :as gen]
                                [properties :as prop])
            [midje.sweet :refer :all]
            [org.dthume.data.interval-treeset :as it]
            [org.dthume.data.interval-treeset.selection :as sel]
            [org.dthume.data.set :as set]))

(defn iv-compare
  [[as ae :as a] [bs be :as b]]
  (let [s (compare as bs)]
    (if (zero? s)
      (let [e (- (compare ae be))]
        (if (zero? e)
          (compare a b)
          e))
      s)))

(def empty-set (it/interval-treeset))

(defn ts [items]
  (if (empty? items) empty-set
      (into empty-set items)))

(defn sorted-iv-set
  [items]
  (into (sorted-set-by iv-compare) items))

(defn ascending?
  [coll]
  (every? (every-pred (fn [[a b]] (not (pos? (iv-compare a b))))
                      identity)
          (partition 2 1 coll)))

(defn equal-elems?
  [a b]
  (every? identity (map = a b)))

(defn gen-intervals
  []
  (->> (gen/tuple gen/int gen/s-pos-int)
       (gen/fmap (fn [[s l]] [s (+ s l)]))
       gen/vector
       gen/not-empty))

(defspec elements-are-correctly-ordered-after-into 100
  (prop/for-all [v (gen-intervals)]
    (let [t (ts v)]
      (ascending? t))))

(defspec union-same-as-clojure-set 100
  (prop/for-all [a (gen-intervals)
                 b (gen-intervals)]
    (let [it  (it/it-union (ts a) (ts b))
          is  (clojure.set/union (sorted-iv-set a) (sorted-iv-set b))]
      (equal-elems? it is))))

