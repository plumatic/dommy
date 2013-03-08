(ns dommy.core-test
  (:use-macros
   [cljs-test.macros :only [is is= deftest]]
   [dommy.macros :only [sel sel1 node]])
  (:require
   [cljs-test.core :as test]
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

(deftest insert-before!
  (let [container (node [:div])
        placemark (node [:span "placemark"])
        el (node [:span "test"])]
    (dommy/append! container placemark)
    (let [res (dommy/insert-before! el placemark)]
      (is= 2 (.-childElementCount container))
      (is= el (.-firstChild container))
      (is= res el))
    (let [res (dommy/insert-before! "text node" placemark)]
      (is= 2 (.-childElementCount container))
      (is= 3 (-> container .-childNodes .-length))
      (is= "text node" (.-textContent (aget (.-childNodes container) 1)))
      (is= res (aget (.-childNodes container) 1)))
    (let [res (dommy/insert-before! [:b "-elem insert-before!"] placemark)]
      (is= 3 (.-childElementCount container))
      (is= "B" (-> res .-tagName))
      (is= res (aget (.-childNodes container) 2)))))

(deftest insert-after!
  (let [container (node [:div])
        placemark (node [:span "placemark"])
        el (node [:span "test"])]
    (dommy/prepend! container placemark)
    (let [res (dommy/insert-after! el placemark)]
      (is= 2 (.-childElementCount container))
      (is= el (.-lastChild container))
      (is= res el))
    (let [res (dommy/insert-after! "text node" placemark)]
      (is= 2 (.-childElementCount container))
      (is= 3 (-> container .-childNodes .-length))
      (is= "text node" (.-textContent (aget (.-childNodes container) 1)))
      (is= res (aget (.-childNodes container) 1)))
    (let [res (dommy/insert-after! [:b "-elem insert-after!"] placemark)]
      (is= 3 (.-childElementCount container))
      (is= "B" (-> res .-tagName))
      (is= res (aget (.-childNodes container) 1)))))

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
  "Only works when `node` is in the DOM"
  [node event-type]
  (if (.-createEvent js/document)
    (let [event (.createEvent js/document "Event")]
      (.initEvent event (name event-type) true true)
      (.dispatchEvent node event))
    (.fireEvent node (str "on" (name event-type))
                (.createEventObject js/document))))

(deftest listener-simple
  (let [el-simple (node [:div#id])
        click-cnt (atom 0)]
    (dommy/listen! el-simple :click (fn [e] #_(js* "debugger") (swap! click-cnt inc)))
    (is= 0 @click-cnt)
    (fire! el-simple :click)
    (is= 1 @click-cnt)))

(deftest closest
  (let [parent (node [:.parent [:.child [:.grandchild]]])
        child (sel1 parent :.child)
        grandchild (sel1 parent :.grandchild)]
    (is= grandchild (dommy/closest parent grandchild :.grandchild))
    (is= child (dommy/closest parent grandchild :.child))
    (is (nil? (dommy/closest parent grandchild :.parent)))
    (dommy/append! js/document.body parent)
    (is= parent (dommy/closest grandchild :.parent))))

(deftest live-listener
  (let [parent (node [:.parent [:.child [:.grandchild]]])
        child (sel1 parent :.child)
        grandchild (sel1 parent :.grandchild)
        click-cnt (atom 0)
        fake-event (js-obj "target" grandchild)]
    ((dommy/live-listener
      parent :.grandchild
      (fn [event]
        (swap! click-cnt inc)
        (is= grandchild (.-selectedTarget event))))
     fake-event)
    (is= 1 @click-cnt)
    ((dommy/live-listener
      parent :.child
      (fn [event]
        (swap! click-cnt inc)
        (is= child (.-selectedTarget event))))
     fake-event)
    (is= 2 @click-cnt)
    ((dommy/live-listener
      parent :.parent
      #(swap! click-cnt inc))
     fake-event)
    (is= 2 @click-cnt)))

(deftest listen!
  (let [el-simple (node [:div#id])
        click-cnt (atom 0)]
    (dommy/listen! el-simple :click (fn [e] #_(js* "debugger") (swap! click-cnt inc)))
    (is= 0 @click-cnt)
    (fire! el-simple :click)
    (is= 1 @click-cnt)))

(deftest listen!-multi
  (let [el (node :.test)
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/listen! el :click listener :dblclick listener)
    (is= 0 @click-cnt)
    (fire! el :click)
    (is= 1 @click-cnt)
    (fire! el :dblclick)
    (is= 2 @click-cnt)))

(deftest unlisten!
  (let [el-nested (node [:ul [:li "Test"]])
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el-nested)
    (dommy/listen! [el-nested :li] :click listener)
    (fire! (sel1 el-nested :li) :click)
    (is= 1 @click-cnt)
    (dommy/unlisten! [el-nested :li] :click listener)
    (fire! (sel1 el-nested :li) :click)
    (is= 1 @click-cnt)
    (dommy/listen! [el-nested :li] :click listener)
    (fire! (sel1 el-nested :li) :click)
    (is= 2 @click-cnt)
    (let [el-nested (node [:.parent [:.child [:.grandchild]]])]
      (dommy/append! js/document.body el-nested)
      (dommy/listen! [el-nested :.child :.grandchild] :click listener)
      (fire! (sel1 el-nested :.grandchild) :click)
      (is= 3 @click-cnt)
      (dommy/unlisten! [el-nested :.child :.grandchild] :click listener)
      (fire! (sel1 el-nested [:.child :.grandchild]) :click)
      (is= 3 @click-cnt))))

(deftest unlisten!-multi
  (let [el (node :.test)
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el)
    (dommy/listen! el :click listener :dblclick listener)
    (fire! el :click)
    (fire! el :dblclick)
    (is= 2 @click-cnt)
    (dommy/unlisten! el :click listener :dblclick listener)
    (fire! el :click)
    (fire! el :dblclick)
    (is= 2 @click-cnt)))

(deftest listen-once!
  (let [el (node :.test)
        click-cnt (atom 0)]
    (dommy/listen-once! el :click #(swap! click-cnt inc))
    (fire! el :click)
    (is= 1 @click-cnt)
    (fire! el :click)
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

(deftest set-style!
  (let [el (node [:div])]
    (dommy/set-style! el :height "0px" :width "1px" :zIndex 1)
    (is= (-> el .-style .-height) "0px")
    (is= (-> el .-style .-width) "1px")
    (is= (-> el .-style .-zIndex) "1")))

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
