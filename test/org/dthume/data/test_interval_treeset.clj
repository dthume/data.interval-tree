(ns org.dthume.data.test-interval-treeset
  (:require [midje.sweet :refer :all]
            [org.dthume.data.interval-treeset :as it]
            [org.dthume.data.interval-treeset.selection :as sel]))

(def ts1 (into (it/interval-treeset) [[0 5] [6 10] [11 15] [16 21] [11 21] [22 25]]))

(def ts2 (into (it/interval-treeset :as-interval :span)
               [{:span [0 5]   :key :a}
                {:span [6 10]  :key :b}
                {:span [11 15] :key :c}
                {:span [11 21] :key :d}
                {:span [16 21] :key :e}
                {:span [22 25] :key :f}]))

(fact "Interval trees support basic seq operations"
  (count ts1)                             => 6 
  (first ts1)                             => [0 5]
  (second ts1)                            => [6 10]
  (conj ts1 [3 6])                        => [[0 5] [3 6] [6 10] [11 15]
                                                [11 21] [16 21] [22 25]])

(fact "Interval trees support indexed operations"
  (nth ts1 1)                             => [6 10]
  (nth ts1 3)                             => [11 21]
  (contains? ts1 [11 21])                 => true)

(fact "Interval trees support associative operations"
  (get ts1 [11 21])                       => [11 21]
  (get ts2 {:span [11 15] :key :c})       => {:span [11 15] :key :c}
  (disj ts1 [11 21])                      => [[0 5] [6 10] [11 15] [16 21] [22 25]]
  (count (disj ts2 {:span [11 15]
                    :key :c}))            => 5)

(fact "Interval trees support selection of overlapping subregions"
  (sel/overlapping-subset ts1 [11 20])    => [[11 15] [11 21] [16 21]]
  (sel/overlapping-subset ts1 [6 8])      => [[6 10]]
  (it/first-overlapping ts1 [11 21])      => [11 15]
  (it/first-overlapping ts2 [11 21])      => {:span [11 15] :key :c}
  (it/select-overlapping ts1 [6 8])       => [[[0 5]]
                                             [[6 10]]
                                             [[11 15] [11 21] [16 21] [22 25]]]
  (-> ts1
      (it/select-overlapping [6 8])
      (sel/disj-prefix))                  => [[6 10] [11 15] [11 21] [16 21] [22 25]] 
  (-> ts1
      (it/select-overlapping [11 20])
      (sel/disj-selected))                => [[0 5] [6 10] [22 25]]
  (-> ts1
      (it/select-overlapping [6 8])
      (sel/disj-suffix))                  => [[0 5] [6 10]] 
  (-> ts1
      (it/select-overlapping [6 10])
      (sel/expandl 1)
      (sel/selected))                     => [[0 5] [6 10]]
  (-> ts1
      (it/select-overlapping [6 10])
      (sel/expandl 1)
      (sel/expandr 1)
      (sel/selected))                     => [[0 5] [6 10] [11 15]]
  (-> ts1
      (it/select-overlapping [11 20])
      (sel/contractl 1)
      (sel/contractr 1)
      (sel/selected))                     => [[11 21]]
  (-> ts1
      (it/select [6 10])
      (sel/selected))                     => [[6 10]]
  (as-> ts1 x
        (it/select x [30 40])
        (mapv count x))                   => [6 0 0]
  (-> ts1
      (it/select [30 40])
      (sel/prefix)
      (count))
  (-> ts2
      (it/select-overlapping [6 10])
      (sel/expandr-while #(not= :d (:key %)))
      (sel/selected))                     => [{:span [6 10] :key :b}
                                             {:span [11 15] :key :c}])

