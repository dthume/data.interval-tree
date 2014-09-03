(ns org.dthume.data.test-check-interval-treeset
  (:require [clojure.core.reducers :as r]
            [clojure.test.check :as tc]
            (clojure.test.check [clojure-test :refer [defspec]]
                                [generators :as gen]
                                [properties :as prop])
            [collection-check :refer [assert-equivalent-collections
                                      assert-equivalent-sets]]
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

(defn ss
  [items]
  (into (sorted-set-by iv-compare) items))

(defn assert-equivalent-indexeds
  [a b]
  (assert (= (map #(nth a %) (range (count a)))
             (map #(nth b %) (range (count b)))))
  true)

(defn assert-equivalent-reversibles
  [a b]
  (assert (every? identity (map = (rseq a) (rseq b))))
  true)

(defn assert-equivalent-stacks
  [a b]
  (assert (= (count a) (count b)))
  (loop [a a b b]
    (when (seq a)
      (do
        (assert (= (peek a) (peek b)))
        (recur (pop a) (pop b)))))
  true)

(defn ascending?
  [coll]
  (every? (every-pred (fn [[a b]] (not (pos? (iv-compare a b))))
                      identity)
          (partition 2 1 coll)))

;; Generators

(def gen-interval
  (->> (gen/tuple gen/int gen/s-pos-int)
       (gen/fmap (fn [[s l]] [s (+ s l)]))))

(def gen-intervals
  (->> gen-interval gen/vector gen/not-empty))

(def gen-commands
  (-> (gen/elements [:disj :conj :union :difference :intersection :into])
      (gen/tuple gen-intervals)
      gen/vector))

(defn- apply-command
  [as-set s [cmd args]]
  (case cmd
    :disj         (apply disj s args)
    :conj         (apply conj s args)
    :into         (into s args)
    :union        (set/union s (as-set args))
    :intersection (set/intersection s (as-set args))
    :difference   (set/difference s (as-set args))))

(defn- apply-commands
  [as-set s cmds]
  (reduce (partial apply-command as-set) s cmds))

(defspec elements-are-correctly-ordered-after-into 100
  (prop/for-all [v gen-intervals]
    (let [t (ts v)]
      (ascending? t))))

(defspec command-set-same-as-clojure-set 1000
  (prop/for-all [a    gen-intervals
                 cmds gen-commands]
    (let [it  (apply-commands ts (ts a) cmds)
          is  (apply-commands ss (ss a) cmds)
          isv (into [] is)]
      (assert-equivalent-sets it is)
      (assert-equivalent-indexeds it isv)
      (assert-equivalent-reversibles it isv)
      (assert-equivalent-stacks it isv)
      true)))


