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

(deftest descendant?
  (let [grandchild (node :.grandchild)
        child (node [:.child grandchild])
        sibling (node :.sibling)
        parent (node [:.parent child sibling])]
    (doseq [contains? [true false]]
      (when-not contains?
        (doseq [n [grandchild child sibling parent]]
          (set! (.-contains n) nil)))
      (is (dommy/descendant? child parent))
      (is (dommy/descendant? grandchild parent))
      (is (not (dommy/descendant? parent child)))
      (is (not (dommy/descendant? parent grandchild)))
      (is (not (dommy/descendant? grandchild sibling)))
      (is (not (dommy/descendant? child sibling))))))

(defn fire!
  "Creates an event of type `event-type`, optionally having
   `update-event!` mutate and return an updated event object,
   and fires it on `node`.
   Only works when `node` is in the DOM"
  [node event-type & [update-event!]]
  (assert (dommy/descendant? node js/document.documentElement))
  (let [update-event! (or update-event! identity)]
    (if (.-createEvent js/document)
      (let [event (.createEvent js/document "Event")]
        (.initEvent event (name event-type) true true)
        (.dispatchEvent node (update-event! event)))
      (.fireEvent node (str "on" (name event-type))
                  (update-event! (.createEventObject js/document))))))

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
    (dommy/append! js/document.body el-simple)
    (dommy/listen! el-simple :click (fn [e] #_(js* "debugger") (swap! click-cnt inc)))
    (is= 0 @click-cnt)
    (fire! el-simple :click)
    (is= 1 @click-cnt)))

(deftest listen!-multi
  (let [el (node :.test)
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el)
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
    (dommy/append! js/document.body el)
    (dommy/listen-once! el :click #(swap! click-cnt inc))
    (fire! el :click)
    (is= 1 @click-cnt)
    (fire! el :click)
    (is= 1 @click-cnt)))

(deftest mouseenter-and-mouseleave
  (let [greatgrandchild (node :.greatgrandchild)
        grandchild (node [:.grandchild greatgrandchild])
        child (node [:.child grandchild])
        sibling (node :.sibling)
        parent (node [:.parent child sibling])
        counter (atom 0)
        listener #(swap! counter inc)
        fire!-called-listener?
        (fn [evt-type relatedTarget]
          (let [orig-count @counter]
            (fire! child evt-type
                   #(doto % (aset "relatedTarget" relatedTarget)))
            (is (some #(= @counter (% orig-count)) [identity inc]))
            (= @counter (inc orig-count))))
        should-call-listener {"outside" nil
                              "sibling" sibling
                              "parent" parent}
        shouldnt-call-listener {"grandchild" grandchild
                                "greatgrandchild" greatgrandchild}]
    (dommy/append! js/document.body parent)
    (doseq [[fake-evt real-evt] {:mouseenter :mouseover, :mouseleave :mouseout}]
      (dommy/listen! child fake-evt listener)
      (doseq [[where relatedTarget] should-call-listener]
        (is (fire!-called-listener? real-evt relatedTarget)
            (format "%s to/from %s is %s" (name real-evt) where (name fake-evt))))
      (doseq [[where relatedTarget] shouldnt-call-listener]
        (is (not (fire!-called-listener? real-evt relatedTarget))
            (format "%s to/from %s isn't %s" (name real-evt) where (name fake-evt))))
      (dommy/unlisten! child fake-evt listener)
      (doseq [[where relatedTarget] (concat should-call-listener shouldnt-call-listener)]
        (is (not (fire!-called-listener? real-evt relatedTarget))
            "after unlisten!-ed, listener never called")))))

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
