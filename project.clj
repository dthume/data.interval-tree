(defproject org.dthume/data.interval-treeset "0.1.3-SNAPSHOT"
  :description "Interval Treeset based on finger trees."
  :url "http://github.com/dthume/data.interval-tree"
  :license {:name "Eclipse Public License 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :scm {:name "git"
        :url "github.com/dthume/data.interval-tree"}

  :plugins [[codox "0.8.9"]
            [lein-marginalia "0.7.1"]
            [lein-midje "3.2.1"]
            [perforate "0.3.4"]]

  :codox {:defaults {:doc/format :markdown}
          :output-dir "doc/codox"}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.finger-tree "0.0.3"]
                 [org.dthume/data.set "0.1.1"]]

  :javac-options ["-target" "1.6" "-source" "1.6"]

  :perforate
  {:benchmark-paths ["src/benchmark/clj"]}

  :profiles
  {:dev
   {:source-paths ["src/dev/clj"]
    :dependencies [[midje "1.9.9"]
                   [perforate "0.3.4"]
                   [org.clojure/test.check "1.1.0"]
                   [collection-check "0.1.6"]]}

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
