(ns ^{:doc "Interval treeset subset selection"}
  org.dthume.data.interval-treeset.selection
  (:refer-clojure :exclude [-> ->> as->])
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

(defn selection
  "Create a selection from 3 trees: `pre`fix `sel`ected and `suff`ix."
  [pre sel suff]
  (tuple pre sel suff))

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
  "Edit a component part of `t` using `lens` (one of `[[prefix]]`,
`[[selected]]` or `[[suffix]]`).
`f` will be called with the value of the component part as the
first argument, followed by any remaining `args`. The result will be converted
to an interval treeset as described below, which will be used as the new value
of the component part, and an updated selection region will be returned.

The result of `f` can be either `nil`, an interval treeset, or any sequential
type:

* `nil` will result in an empty interval treeset as the component part value.
* An interval treeset will be used as is as the component part value.
* Any other sequential type will be poured into an empty interval treeset
  (created from the original) which will be used as the component part value."
  [t lens f & args]
  (let [old (lens t)
        fd (apply f old args)]
    (clojure.core/->>
     (cond
      (nil? fd)                            (empty old)
      (not (instance? IntervalTreeSet fd)) (into (empty old) fd)
      :else                                fd)
     (lens t))))

(defn transform
  "Like `[[edit]]`, but passes the component value as the _last_ argument to
`f` rather than the first."
  [t lens f & args]
  (let [old (lens t)
        fd  (clojure.core/->> [old] (concat args) (apply f))]
    (clojure.core/->>
     (cond
      (nil? fd)                            (empty old)
      (not (instance? IntervalTreeSet fd)) (into (empty old) fd)
      :else                                fd)
     (lens t))))

(defmacro ->
  "Analogue of `clojure.core/->` for use with selections.

Takes a selected region `t`, a `lens` (one of `[[prefix]]`, `[[selected]]` or
`[[suffix]]`) and a body of expressions, and threads the component part value
identified by `lens` through `body` with `clojure.core/->`. The result is
convered to an interval treeset as per `[[edit]]`, and used as the new
component part value."
  [t lens & body]
  `(transform ~t ~lens (fn [x#] (clojure.core/-> x# ~@body))))

(defmacro ->>
  "Analogue of `clojure.core/->>` for use with selections.

Takes a selected region `t`, a `lens` (one of `[[prefix]]`, `[[selected]]` or
`[[suffix]]`) and a body of expressions, and threads the component part value
identified by `lens` through `body` with `clojure.core/->>`. The result is
convered to an interval treeset as per `[[edit]]`, and used as the new
component part value."
  [t lens & body]
  `(transform ~t ~lens (fn [x#] (clojure.core/->> x# ~@body))))

(defmacro as->
  "Analogue of `clojure.core/as->` for use with selections.

Takes a selected region `t`, a `lens` (one of `[[prefix]]`, `[[selected]]` or
`[[suffix]]`) and a body of expressions, and threads the component part value
identified by `lens` through `body` with `clojure.core/as->`. The result is
convered to an interval treeset as per `[[edit]]`, and used as the new
component part value."
  [t lens n & body]
  `(transform ~t ~lens (fn [x#] (clojure.core/as-> x# ~n ~@body))))

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
             (clojure.core/-> r (ft/conjl (peek p)) pop)
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
             (clojure.core/-> r (ft/conjl (peek p)) pop)
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
             (clojure.core/-> r next (conj (peek s)))
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
             (clojure.core/-> r next (conj (peek s)))
             (next s)))))

(defn slide-right
  "Slide the covered region right by `n` items"
  [t n]
  (slide-right-by (constantly true) n))

(defn disj-selected
  "Return an interval treeset with only `prefix` and `suffix`."
  [t]
  (it/union (prefix t) (suffix t)))

(defn disj-prefix
  "Return an interval treeset with only `region` and `suffix`."
  [t]
  (it/union (selected t) (suffix t)))

(defn disj-suffix
  "Return an interval treeset with only `prefix` and `region`."
  [t]
  (it/union (prefix t) (selected t)))

(defn unselect
  "Combine `prefix`, `region` and `suffix` back together to form an interval
treeset."
  [t]
  (apply it/union t))

(defn overlapping-subset
  "Search `this` for all values which overlap `ival`."
  [ts ival]
  (clojure.core/-> ts
      (it/select-overlapping ival)
      selected))
