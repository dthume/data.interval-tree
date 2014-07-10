# Interval Treeset

Interval Treeset built on [data.finger-tree](https://github.com/clojure/data.finger-tree).

Largely based on the implementation described in the
[original paper](http://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf).

## Leiningen

```clojure
[org.dthume/data.interval-treeset "0.1.0-SNAPSHOT"]
```

## Basic Usage

```clojure
;; just for this documentation
(require '[org.dthume.data.finger-tree :as ft])
```

Interval Treesets sort their entries by _interval_. Intervals are simply pairs
of values, and you can use vectors to represent them:

```clojure
[5 10] ; the interval 5 - 10
```

However you may find it convienient to use the `interval` function which
produces an optimised (but still compatible) interval:

```clojure
(ft/interval 5 10)
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
(def ts (into (ft/interval-treeset) [[0 2] [3 5] [6 8]]))
;; => ([0 2] [3 5] [6 8])
```

Items will be added at the correct position:

```clojure
(conj ts [1 4]) ;; Amortized O(1)
;; => ([0 2] [1 4] [3 5] [6 8])
```

Adding an existing item is a noop:

```clojure
(conj ts [0 2])
;; => ([0 2] [3 5] [6 8])
```

```clojure
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
(require '[org.dthume.data.finger-tree.selection :as sel)
```

## License

Copyright (C) 2014 David Thomas Hume.
Distributed under the Eclipse Public License, the same as Clojure.