(defproject prismatic/dommy "1.0.0"
  :clojurescript? true
  :description "Clojurescript DOM manipulation"
  :url "https://github.com/prismatic/dommy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:url "git@github.com:prismatic/dommy.git"}
  :pom-addition [:developers [:developer
                              [:name "Prismatic"]
                              [:url "http://getprismatic.com"]
                              [:email "admin+oss@getprismatic.com"]
                              [:timezone "-8"]]]

  :dependencies [[org.clojure/clojure "1.6.0" :scope "provided"]
                 [org.clojure/clojurescript "0.0-2356" :scope "provided"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :profiles {:dev {:dependencies [[com.cemerick/clojurescript.test "0.3.1"]]
                   :plugins [[com.cemerick/clojurescript.test "0.3.1"]]}}

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
