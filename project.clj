(defproject org.dthume/data.interval-treeset "0.1.0-SNAPSHOT"
  :description "Interval Treeset based on finger trees."

  :plugins [[codox "0.8.9"]
            [lein-marginalia "0.7.1"]
            [lein-midje "3.0.0"]
            [perforate "0.3.3"]]

  :codox {:defaults {:doc/format :markdown}
          :output-dir "doc/codox"}

  :dependencies [[clj-tuple "0.1.5"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/data.finger-tree "0.0.2"]]

  :javac-options ["-target" "1.6" "-source" "1.6"]

  :perforate
  {:benchmark-paths ["src/benchmark/clj"]}

  :profiles
  {:dev
   {:source-paths ["src/dev/clj"]
    :dependencies [[midje "1.6.3"]
                   [perforate "0.3.3"]]}

   :benchmark
   {:jvm-opts ^:replace
    ["-XX:+DoEscapeAnalysis"
     "-XX:+UseBiasedLocking"]}

   :site {}}

  :aliases
  {"dev-bench"
   ^{:doc "Run development benchmarks"}
   ["with-profile" "benchmark" "perforate"]

   "dev-repl"
   ^{:doc "Start a clean development NREPL session"}
   ["do" ["clean"] ["repl"]]

   "dev-test"
   ^{:doc "Run development unit tests"}
   ["do" ["clean"] ["midje"]]

   "dev-doc"
   ^{:doc "Start a clean development NREPL session"}
   ["with-profile" "site"
    ["do"
     ["clean"]
     ["doc"]
     ["marg"
      "--dir" "doc/walkthrough"
      "--name" "data.interval-treeset Walkthrough"
      "--desc" "An introduction to data.interval-treeset"
      "--file" "walkthrough"
      "test/org/dthume/data/test_interval_treeset.clj"]]]})
