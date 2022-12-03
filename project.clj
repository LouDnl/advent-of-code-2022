(defproject advent-of-code-2022 "0.1.0-SNAPSHOT"
  :description "Project for the advent of code 2022"
  :url "https://adventofcode.com/"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.11.1"]  ;; Clojure all by itself (https://github.com/clojure/clojure | https://clojure.org/guides/deps_and_cli)
                 [org.clojure/clojurescript "1.11.60" :scope "provided"]  ;; ClojureScript all by itself (https://github.com/clojure/clojurescript)

                 ;; Data
                 [org.clojure/data.csv "1.0.1"]  ;; CSV reader/writer to/from (https://github.com/clojure/data.csv)
                 [org.clojure/data.xml "0.2.0-alpha8"]  ;; library for reading and writing XML data (https://github.com/clojure/data.xml)
                 [cheshire "5.11.0"]  ;; JSON encoding (https://github.com/dakrone/cheshire)

                 ;; Date/Time
                 [clojure.java-time "1.1.0"]  ;; A Clojure wrapper for Java 8 Date-Time API (https://github.com/dm3/clojure.java-time)

                 ;; Configuration and state management
                 [environ "1.2.0"]]  ;; Manage environtment settings (https://github.com/weavejester/environ)

  :plugins [[lein-ancient "1.0.0-RC4-SNAPSHOT"]  ;; plugin to check your project for outdated dependencies and plugins, as well as upgrade them if desired (https://github.com/xsc/lein-ancient)
            [lein-environ "1.2.0"]]  ;; plugin for the environ dependency to draw settings from the lein project map

  :repositories [["jitpack" "https://jitpack.io"]]

  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [;; Hot loading dependencies
                                  [clj-commons/pomegranate "1.2.1"]]
                   :env {:environment "development"}}}

  :source-paths ["src/clj" "src/cljc" "src/cljs"]

  :aliases {"main" ["run" "-m" "advent-of-code-2022.core"]}

  :main ^:skip-aot advent-of-code-2022.core

  :target-path "target/%s")
