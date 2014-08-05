(defproject prismatic/dommy "0.1.4-SNAPSHOT"
  :clojurescript? true
  :description "Clojurescript DOM templating and manipulation"
  :url "https://github.com/prismatic/dommy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:url "git@github.com:prismatic/dommy.git"}
  :pom-addition [:developers [:developer
                              [:name "Prismatic"]
                              [:url "http://getprismatic.com"]
                              [:email "admin+oss@getprismatic.com"]
                              [:timezone "-8"]]]
  :plugins [[lein-cljsbuild "0.3.2"]
            [com.cemerick/clojurescript.test "0.2.1"]
            [com.cemerick/austin "0.1.3"]]

  :hooks [leiningen.cljsbuild]

  :profiles {:dev {:dependencies [[crate "0.2.3"] ;; for perf test
                                  [com.cemerick/clojurescript.test "0.2.1"]]}}

  :lein-release {:deploy-via :shell
                 :shell ["lein" "deploy" "clojars"]}

  :cljsbuild
  {:builds
   {:test {:source-paths ["src" "test"]
           :incremental? true
           :compiler {:output-to "target/unit-test.js"
                      :optimizations :whitespace
                      :pretty-print true}}}
   :test-commands {"unit" ["phantomjs" :runner
                           "window.literal_js_was_evaluated=true"
                           "target/unit-test.js"]}})
