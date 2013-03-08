(defproject prismatic/dommy "0.0.3-SNAPSHOT"
  :clojurescript? true
  :description "No nonsense Clojurescript dom templating and (soon) manipulation"
  :url "https://github.com/prismatic/dommy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:url "git@github.com:prismatic/dommy.git"}
  :pom-addition [:developers [:developer
                              [:name "Prismatic"]
                              [:url "http://getprismatic.com"]
                              [:email "admin+oss@getprismatic.com"]
                              [:timezone "-8"]]]
  :plugins [[lein-cljsbuild "0.3.0"]]
  :hooks [leiningen.cljsbuild]
  :dependencies [[crate "0.2.3" :scope "dev"] ;; for perf test
                 [prismatic/cljs-test "0.0.4"]]
  :cljsbuild
    {:builds
     {:dev {:source-paths ["src"]
            :compiler {:output-to "target/main.js"
                       :optimizations :whitespace
                       :pretty-print true}}
      :test {:source-paths ["test"]
             :compiler {:output-to "target/unit-test.js"
                        :optimizations :whitespace
                        :pretty-print true}}}
     :test-commands {"unit" ["phantomjs" "target/unit-test.js" "resources/test.html"]}})
