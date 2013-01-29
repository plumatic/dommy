(ns dommy.core-test
  (:use-macros [dommy.core-compile :only [sel sel1]]
               [dommy.template-compile :only [node]])
  (:require [dommy.core :as dommy]))

(def body js/document.body)

(defn sel-test []
  ;; Simple
  (dommy/append! body (node [:div#foo]))
  (assert (.-tagName (sel1 :#foo)) "DIV")
  ;; Evaluated
  (let [bar :#bar
        bar-el (node bar)]
    (dommy/append! body bar-el)
    (assert (identical? (sel1 bar) bar-el)))
  ;; Nested
  
  (.log js/console "PASS core-test/simple-test"))

(defn class-test []
  (let [el-simple (node [:div.foo])
        el-complex (node [:div.c1.c.c3.c.c4.d?])]
    (assert (dommy/has-class? el-simple "foo"))
    (assert (dommy/has-class? el-complex "c"))
    (assert (not (dommy/has-class? el-complex "c5")))
    (dommy/add-class! el-simple "test")
    (assert (dommy/has-class? el-simple "test"))
    (dommy/remove-class! el-simple "test")
    (assert (not (dommy/has-class? el-simple "test")))
    (dommy/toggle-class! el-simple "test")
    (js/console.log (.-className el-simple))
    (js/console.log (dommy/has-class? el-simple "test"))
    (assert (dommy/has-class? el-simple "test")))
  (.log js/console "PASS core-test/class-test"))

(defn class-perf-test [node]
  (let [start (.now js/Date)]        
    (dotimes [i 1e6]
      (dommy/has-class? node (str "class" (inc (mod i 10))))
      (dommy/toggle-class! node (str "class" (inc (mod i 10)))))
    (/ (- (.now js/Date) start) 1000)))

(defn jquery-perf-test [node]
  (let [node (js/jQuery node)
        start (.now js/Date)]        
    (dotimes [i 1e6]
      (.hasClass node (str "class" (inc (mod i 10))))
      (.toggleClass node (str "class" (inc (mod i 10)))))
    (/ (- (.now js/Date) start) 1000)))

(defn ^:export class-perf []
  (let [node (node [:div.class1.class2.class3.class4.class5])]
    (.log js/console (pr-str { :dommy  (class-perf-test node)
                              :jquery (jquery-perf-test node)}))))
 
(sel-test)
(class-test)