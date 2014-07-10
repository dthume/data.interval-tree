(ns ^{:doc "Interval treeset subset selection"}
  org.dthume.data.interval-treeset.selection
  (:require [clj-tuple :refer (tuple)]
            [clojure.core.reducers :as r]
            [clojure.data.finger-tree :as ft]
            [org.dthume.data.interval-treeset :as it])
  (:import [org.dthume.data.interval_treeset IntervalTreeSet]))

(defn- with-tree
  [^IntervalTreeSet base tree]
  (IntervalTreeSet. (.as-interval base)
                    (.compare-point base)
                    tree
                    (.mdata base)))

(defn selected
  "Return only the selected part of this region in the one arg case, set
the selected part in the two arg case."
  (^IntervalTreeSet [t] (nth t 1))
  ([t v] (assoc t 1 v)))

(defn prefix
  "Return only the prefix part of this region in the one arg case, set
the prefix part in the two arg case."
  (^IntervalTreeSet [t] (nth t 0))
  ([t v] (assoc t 0 v)))

(defn suffix
  "Return only the suffix part of this region in the one arg case, set
the suffix part in the two arg case."
  (^IntervalTreeSet [t] (nth t 2))
  ([t v] (assoc t 2 v)))

(defn edit
  [lens t f & args]
  (lens t (apply f (lens t) args)))

(defn each
  [lens t f & args]
  (let [old (lens t)
        updated (into (empty old) (r/map #(apply f % args) old))]
    (lens t updated)))

(for [s ['prefix 'selected 'suffix]
      :let [n (name s)
            each-n (symbol (str "each-" n))
            edit-n (symbol (str "edit-" n))]]
  `(do
     (def ~each-n (partial each ~s))
     (def ~edit-n (partial each ~s))))

(defn contractl-by
  "Contract the covered region to the left until `pred` has returned logical
`true` `n` times."
  [t pred n]
  (loop [p (prefix t) rs (selected t) n n]
    (if (and (some? rs) (pos? n))
      (let [r (first rs)]
        (recur (conj p r) (next rs) (if (pred r) (dec n) n)))
      (tuple p rs (suffix t)))))

(defn contractl-while
  "Contract the covered region to the left while `pred` returns logical `true`."
  [t pred]
  (loop [p (prefix t) rs (selected t)]
    (if (and (some? rs) (pred (first rs)))
      (recur (conj p (first rs)) (next rs))
      (tuple p rs (suffix t)))))

(defn contractl
  "Contract the covered region to the left by `n` items."
  [t n]
  (contractl-by t (constantly true) n))

(defn expandl-by
  "Expand the covered region to the left until `pred` has returned logical
`true` `n` times."
  [t pred n]
  (loop [p (.tree (prefix t)) r (.tree (selected t)) n n]
    (if (or (empty? p) (zero? n))
      (tuple (with-tree (prefix t) p)
             (with-tree (selected t) r)
             (suffix t))
      (recur (pop p) (ft/conjl r (peek p))
             (if (pred (peek p)) (dec n) n)))))

(defn expandl-while
  "Expand the covered region to the left while `pred` returns logical `true`."
  [t pred]
  (loop [p (.tree (prefix t)) r (.tree (selected t))]
    (if (or (empty? p) (pred (peek p)))
      (tuple (with-tree (prefix t) p)
             (with-tree (selected t) r)
             (suffix t))
      (recur (pop p) (ft/conjl r (peek p))))))

(defn expandl
  "Expand the covered region to the left by `n` items."
  [t n]
  (expandl-by t (constantly true) n))

(defn contractr-by
  "Contract the covered region to the right until `pred` has returned logical
`true` `n` times."
  [t pred n]
  (loop [r (.tree (selected t)) s (.tree (suffix t)) n n]
    (if (or (empty? r) (zero? n))
      (tuple (prefix t)
             (with-tree (selected t) r)
             (with-tree (suffix t) s))
      (recur (pop r) (ft/conjl s (peek r))
             (if (pred (peek r)) (dec n) n)))))

(defn contractr-while
  "Contract the covered region to the right while `pred` returns logical true."
  [t pred]
  (loop [r (.tree (selected t)) s (.tree (selected t))]
    (if (or (empty? r) (pred (peek r)))
      (tuple (prefix t)
             (with-tree (selected t) r)
             (with-tree (suffix t) s))
      (recur (pop r) (ft/conjl s (peek r))))))

(defn contractr
  "Contract the covered region to the right by `n` items."
  [t n]
  (contractr-by t (constantly true) n))

(defn expandr-by
  "Expand the covered region to the right until `pred` has evaluated to logical
`true` `n` times."
  [t pred n]
  (loop [r (.tree (selected t)) ss (.tree (suffix t)) n n]
    (if (and (some? ss) (pos? n))
      (let [s (first ss)]
        (recur (conj r s) (next ss)
               (if (pred s) (dec n) n)))
      (tuple (prefix t)
             (with-tree (selected t) r)
             (with-tree (suffix t) ss)))))

(defn expandr-while
  "Expand the covered region to the right while `pred` evaluates to logical
`true`"
  [t pred]
  (loop [r (.tree (selected t)) ss (.tree (suffix t))]
    (if (or (empty? ss) (not (pred (first ss))))
      (tuple (prefix t)
             (with-tree (selected t) r)
             (with-tree (suffix t) ss))
      (recur (conj r (first ss)) (next ss)))))

(defn expandr
  "Expand the covered region `n` items to the right."
  [t n]
  (expandr-by t (constantly true) n))

(defn slide-left-by
  "Slide the covered region left unti `pred` has returned logical `true`
`n` times."
  [t pred n]
  (loop [p (.tree (prefix t))
         r (.tree (selected t))
         s (.tree (suffix t))
         n n]
    (if (or (empty? p) (zero? n))
      (tuple (with-tree (prefix t) p)
             (with-tree (selected t) r)
             (with-tree (suffix t) s))
      (recur (pop p)
             (-> r (ft/conjl (peek p)) pop)
             (ft/conjl (peek r) s)
             (if (pred (peek p)) (dec n) n)))))

(defn slide-left-while
  "Slide the covered region left while `pred` returns logical `true`."
  [t pred]
  (loop [p (.tree (prefix t))
         r (.tree (selected t))
         s (.tree (suffix t))]
    (if (or (empty? p) (pred (peek p)))
      (tuple (with-tree (prefix t) p)
             (with-tree (selected t) r)
             (with-tree (suffix t) s))
      (recur (pop p)
             (-> r (ft/conjl (peek p)) pop)
             (ft/conjl (peek r) s)))))

(defn slide-left
  "Slide the covered region left by `n` items."
  [t n]
  (slide-left-by t (constantly true) n))

(defn slide-right-by
  "Slide the covered region right until `pred` has returned logical `true`
`n` times."
  [t pred n]
  (loop [p (.tree (prefix t))
         r (.tree (selected t))
         s (.tree (suffix t))
         n n]
    (if (or (empty? s) (zero? n))
      (tuple (with-tree (prefix t) p)
             (with-tree (selected t) r)
             (with-tree (suffix t) s))
      (recur (conj p (first r))
             (-> r next (conj (peek s)))
             (next s)
             (if (pred (first s)) (dec n) n)))))

(defn slide-right-while
  "Slide the covered region right while `pred` returns logical `true`."
  [t pred]
  (loop [p (.tree (prefix t))
         r (.tree (selected t))
         s (.tree (suffix t))]
    (if (or (empty? s) (pred (first s)))
      (tuple (with-tree (prefix t) p)
             (with-tree (selected t) r)
             (with-tree (suffix t) s))
      (recur (conj p (first r))
             (-> r next (conj (peek s)))
             (next s)))))

(defn slide-right
  "Slide the covered region right by `n` items"
  [t n]
  (slide-right-by (constantly true) n))

(defn disj-selected
  "Return an interval treeset with only `prefix` and `suffix`."
  [t]
  (ft/ft-concat (prefix t) (suffix t)))

(defn disj-prefix
  "Return an interval treeset with only `region` and `suffix`."
  [t]
  (ft/ft-concat (selected t) (suffix t)))

(defn disj-suffix
  "Return an interval treeset with only `prefix` and `region`."
  [t]
  (ft/ft-concat (prefix t) (selected t)))

(defn unselect
  "Combine `prefix`, `region` and `suffix` back together to form an interval
treeset."
  [[prefix region suffix]]
  (-> prefix
      (ft/ft-concat region)
      (ft/ft-concat suffix)))

(defn overlapping-subset
  "Search `this` for all values which overlap `ival`."
  [ts ival]
  (-> ts
      (it/select-overlapping ival)
      selected))
