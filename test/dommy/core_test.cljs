(ns dommy.core-test
  (:use-macros
   [cljs-test.core :only [is is= deftest]]
   [dommy.macros :only [sel sel1 node]])
  (:require
   [dommy.core :as dommy]))

(def body js/document.body)

(deftest append!
  (let [container (node [:div])
        el (node [:span "test"])]
    (dommy/append! container el)
    (is= 1 (.-childElementCount container))
    (is= el (.-lastChild container))
    (dommy/append! container "text node")
    (is= 1 (.-childElementCount container))
    (is= 2 (-> container .-childNodes .-length))
    (is= "text node" (.-textContent (aget (.-childNodes container) 1)))
    (dommy/append! container [:b "-elem append!"])
    (is= 2 (.-childElementCount container))
    (is= "B" (-> container .-lastChild .-tagName))))

(deftest prepend!
  (let [container (node [:div])
        el (node [:span "test"])]
    (dommy/prepend! container el)
    (is= 1 (.-childElementCount container))
    (is= el (.-firstChild container))
    (dommy/prepend! container "text node")
    (is= 1 (.-childElementCount container))
    (is= 2 (-> container .-childNodes .-length))
    (is= "text node" (.-textContent (aget (.-childNodes container) 0)))
    (dommy/prepend! container [:b "-elem prepend!"])
    (is= 2 (.-childElementCount container))
    (is= "B" (-> container .-firstChild .-tagName))))

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

(defn fire!
  [node event-type]
  (if (.-createEvent js/document)
    (let [event (.createEvent js/document "Event")]
      (.initEvent event (name event-type) true true)
      (.dispatchEvent node event))
    (.fireEvent node (str "on" (name event-type))
                (.createEventObject js/document))))

(deftest live-listener
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

(deftest listen!
  (let [el-simple (node [:div#id])
        click-cnt (atom 0)]
    (dommy/listen! el-simple :click (fn [e] #_(js* "debugger") (swap! click-cnt inc)))
    (is= 0 @click-cnt)
    (fire! el-simple :click)
    (is= 1 @click-cnt)))

(deftest unlisten!
  (let [el-nested (node [:ul [:li "Test"]])
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el-nested)
    (dommy/listen! el-nested :click :li listener)
    (fire! (sel1 el-nested :li) :click)
    (is= 1 @click-cnt)
    (dommy/unlisten! el-nested :click :li listener)
    (fire! (sel1 el-nested :li) :click)
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
