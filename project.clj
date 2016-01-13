(defproject prismatic/dommy "1.1.1-SNAPSHOT"
  :clojurescript? true
  :description "Clojurescript DOM manipulation"
  :url "https://github.com/plumatic/dommy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0" :scope "provided"]
                 [org.clojure/clojurescript "0.0-2356" :scope "provided"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :hooks [leiningen.cljsbuild]

  :profiles {:dev {:dependencies [[com.cemerick/clojurescript.test "0.3.1"]]
                   :plugins [[com.cemerick/clojurescript.test "0.3.1"]]
                   :cljsbuild
                   {:builds
                    {:test {:source-paths ["src" "test"]
                            :incremental? true
                            :compiler {:output-to "target/unit-test.js"
                                       :optimizations :whitespace
                                       :pretty-print true}}}
                    :test-commands {"unit" ["phantomjs" :runner
                                            "window.literal_js_was_evaluated=true"
                                            "target/unit-test.js"]}}}}

  :lein-release {:deploy-via :shell
                 :shell ["lein" "deploy" "clojars"]}

  :cljsbuild {:builds {}})
