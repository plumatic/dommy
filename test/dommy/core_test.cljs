(ns dommy.core-test
  (:use-macros [dommy.core-compile :only [sel sel1]]
               [dommy.template-compile :only [node]]
               [cljs-test.core :only [is is= deftest]])
  (:require [dommy.core :as dommy]))

(def body js/document.body)

(deftest basic-selection
  ;; Simple
  (dommy/append! body (node [:div#foo]))
  (is= (.-tagName (sel1 :#foo)) "DIV")
  ;; Evaluated
  (let [bar :#bar
        bar-el (node bar)]
    (dommy/append! body bar-el)
    (is= (sel1 bar) bar-el)))

(deftest simple-class
  (let [el-simple (node [:div.foo])
        el-complex (node [:div.c1.c.c3.c.c4.d?])]
    (is (dommy/has-class? el-simple "foo"))
    (is (dommy/has-class? el-complex "c"))
    (is (not (dommy/has-class? el-complex "c5")))
    (dommy/add-class! el-simple "test")
    (is (dommy/has-class? el-simple "test"))
    (dommy/remove-class! el-simple "test")
    (is (not (dommy/has-class? el-simple "test")))
    (dommy/toggle-class! el-simple "test")
    (is (dommy/has-class? el-simple "test"))))

(defn click!
  "Simulates a click event on node"
  [node]
  (let [event (.createEvent js/document "MouseEvents")]
    (.initMouseEvent event "click" true true)
    (.dispatchEvent node event)))

(deftest listener-simple
  (let [el-simple (node [:div#id])
        click-cnt (atom 0)]
    (dommy/listen! el-simple :click (fn [e] #_(js* "debugger") (swap! click-cnt inc)))
    (is= 0 @click-cnt)
    (click! el-simple)
    (is= 1 @click-cnt)))

(deftest listener-live
  (let [el-nested (node [:ul])
        target (node [:li.class1])
        click-cnt (atom 0)
        listener (dommy/live-listener el-nested :li.class1 #(swap! click-cnt inc))
        not-target (node [:li])]
    (is= 0 @click-cnt)
    (dommy/append! el-nested target)
    (dommy/append! el-nested not-target)
    (listener (js-obj "target" target))    
    (is= 1 @click-cnt)
    (listener (js-obj "target" not-target))    
    (is= 1 @click-cnt)))

(deftest toggle!
  (let [el-simple (node [:div])]
    (is (not (dommy/hidden? el-simple)))
    (dommy/toggle! el-simple)
    (is= "none" (-> el-simple .-style .-display))
    (dommy/toggle! el-simple false)
    (is= "none" (-> el-simple .-style .-display))
    (dommy/toggle! el-simple true)
    (is (not (dommy/hidden? el-simple)))))

(deftest ->Array
  (let [array (dommy/->Array (js* "{length: 2, 0: 'lol', 1: 'wut'}"))]
    (is (instance? js/Array array))
    (is= (.-length array) 2)
    (is= (aget array 0) "lol")
    (is= (aget array 1) "wut")))

;; Performance test to run in browser

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
    (.log js/console (pr-str {:dommy  (class-perf-test node)
                              :jquery (jquery-perf-test node)}))))