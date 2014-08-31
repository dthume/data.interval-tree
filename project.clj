(defproject org.dthume/data.interval-treeset "0.1.1-SNAPSHOT"
  :description "Interval Treeset based on finger trees."

  :plugins [[codox "0.8.9"]
            [lein-marginalia "0.7.1"]
            [lein-midje "3.0.0"]
            [perforate "0.3.3"]]

  :codox {:defaults {:doc/format :markdown}
          :output-dir "doc/codox"}

  :dependencies [[clj-tuple "0.1.5"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/data.finger-tree "0.0.2"]
                 [org.dthume/data.set "0.1.0-SNAPSHOT"]]

  :javac-options ["-target" "1.6" "-source" "1.6"]

  :perforate
  {:benchmark-paths ["src/benchmark/clj"]}

  :profiles
  {:dev
   {:source-paths ["src/dev/clj"]
    :dependencies [[midje "1.6.3"]
                   [perforate "0.3.3"]
                   [org.clojure/test.check "0.5.9"]]}

   :benchmark
   {:jvm-opts ^:replace
    ["-XX:+DoEscapeAnalysis"
     "-XX:+UseBiasedLocking"]}

   :site {}}

  :aliases
  {"ci-build"
   ^{:doc "Perform the Continuous Integration build"}
   ["do" ["clean"] ["check"] ["midje"]]
   
   "dev-bench"
   ^{:doc "Run development benchmarks"}
   ["with-profile" "benchmark" "perforate"]

   "dev-repl"
   ^{:doc "Start a clean development NREPL session"}
   ["do" ["clean"] ["repl"]]

   "dev-test"
   ^{:doc "Run development unit tests"}
   ["do" ["clean"] ["midje"]]

   "dev-doc"
   ^{:doc "Generate project documentation"}
   ["with-profile" "site"
    ["do"
     ["clean"]
     ["doc"]
     ["marg"
      "--dir" "doc/walkthrough"
      "--name" "data.interval-treeset Walkthrough"
      "--desc" "An introduction to data.interval-treeset"
      "--file" "walkthrough.html"
      "test/org/dthume/data/test_interval_treeset.clj"]]]})
