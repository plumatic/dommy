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
    (assert (not (dommy/has-class? el-simple "test"))))
  (.log js/console "PASS core-test/class-test"))

(defn class-perf-test [has-class?]
  (let [node (node [:div.class1.class2.class3.class4.class5])
        start (.now js/Date)]
    (dotimes [i 1e5]
      (assert 
         (= (<= (inc (mod i 10)) 5)
            (has-class? node (str "class" (inc (mod i 10)))))
         (format "Checking if %s has class %s on %s"
                 (.-outerHTML node)
                 (str "class" (inc (mod i 10)))
                 has-class?)))
    (/ (- (.now js/Date) start) 1000)))

(defn ^:export class-perf []
  (.log js/console (pr-str { :fast  (class-perf-test dommy/has-class?)})))
 
(sel-test)
(class-test)