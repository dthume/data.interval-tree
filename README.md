# Interval Treeset

Interval Treeset built on [data.finger-tree](https://github.com/clojure/data.finger-tree).

Largely based on the implementation described in the
[original paper](http://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf).

[![Build Status](https://travis-ci.org/dthume/data.interval-tree.svg?branch=master)](https://travis-ci.org/dthume/data.interval-tree)

## Leiningen

[![Clojars Project](http://clojars.org/org.dthume/data.interval-treeset/latest-version.svg)](http://clojars.org/org.dthume/data.set)

## API Docs

[codox](https://github.com/weavejester/codox)
generated documentation can be found
[here](http://dthume.github.io/data.interval-tree/codox/index.html).

A [marginalia](https://github.com/gdeer81/marginalia)
generated walkthrough of how to use the library can be found
[here](http://dthume.github.io/data.interval-tree/walkthrough/walkthrough.html).


## Basic Usage

For a full set of examples, see the walkthrough.

```clojure
;; just for this documentation
(require '[org.dthume.data.interval-treeset :as it])
```

Interval Treesets sort their entries by _interval_. Intervals are simply pairs
of values, and you can use vectors to represent them:

```clojure
[5 10] ; the interval 5 - 10
```

However you may find it convienient to use the `interval` function which
produces an optimised (but still compatible) interval:

```clojure
(it/interval 5 10)
;; => [5 10]
```

This uses [clj-tuple](https://github.com/ztellman/clj-tuple) to
represent the pair.

Treesets can be created using `interval-treeset`. Because treesets take a
number of (optional) configuration options, there is no constructor which
takes items to populate the initial treeset with such as
`clojure.core/vector`. Instead treesets are created using `interval-treeset`
and then filled using `into`:

```clojure
(def ts (into (it/interval-treeset) [[0 2] [3 5] [6 8]]))
;; => ([0 2] [3 5] [6 8])
```

If you wish to store something other than raw intervals in the set, then
provide an interval lensing function:

```clojure
(def cts (into (it/interval-treeset :as-interval :span)
               [{:span [6 8] :id 3} {:span [3 5] :id 2} {:span [0 2] :id 1}]))
;; => ({:span [0 2] :id 1} {:span [3 5] :id 2} {:span [6 8] :id 3})
```

Items will be added at the correct position:

```clojure
(conj ts [1 4]) ;; Amortized O(log(n))
;; => ([0 2] [1 4] [3 5] [6 8])

(count ts) ;; O(1)
;; => 3

(first ts) ;; Amortized O(1)
;; => [0 2]

(peek ts)  ;; Amortized O(1)
;; => [6 8]

(nth ts 1) ;; O(log(n))
;; => [3 5]
```

## Selections

```clojure
(require '[org.dthume.data.interval-treeset.selection :as sel)
```

You should probably _not_ attempt to `use` or `(require ... :refer :all)` the
selection namespace, since it contains vars which shadow `clojure.core`.

## TODO

- Confirm union correct wrt paper.
- Implement more efficient intersection/difference as with union.
- Customize print-method to print as set rather than seq?

## License

Copyright (C) 2014 David Thomas Hume.
Distributed under the Eclipse Public License, the same as Clojure.