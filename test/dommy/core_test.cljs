(ns dommy.core-test
  (:use-macros
   [cljs-test.macros :only [is is= deftest]]
   [dommy.macros :only [sel sel1 node]])
  (:require
   [cljs-test.core :as test]
   [dommy.utils :as utils]
   [dommy.core :as dommy]))

(def body js/document.body)

(deftest set-html!
  (let [el (node [:span "foo" [:b "bar"]])]
    (is= "foo<b>bar</b>" (dommy/html el))
    (dommy/set-html! el "hello <i>world</i>")
    (is= "hello <i>world</i>" (dommy/html el))
    (dommy/set-html! el "")
    (is= "" (dommy/html el))))

(deftest text
  (let [el (node [:span "foo" [:b "bar"]])]
    (is= "foobar" (dommy/text el))
    (dommy/set-style! el :display "none")
    (is= "foobar" (dommy/text el))))

(deftest set-text!
  (let [el (node [:span])]
    (is= "" (dommy/text el))
    (dommy/set-text! el "test")
    (is= "test" (dommy/text el))
    (dommy/set-text! el "<i>woah</i>")
    (is= "<i>woah</i>" (dommy/text el))
    (dommy/set-text! el "")
    (is= "" (dommy/text el))))

(deftest value
  (let [el (node [:select (for [n (range 1 10)] [:option n])])]
    (is= "1" (dommy/value el))))

(deftest set-value!
  (let [el (node [:select (for [n (range 1 10)] [:option n])])]
    (is= "1" (dommy/value el))
    (dommy/set-value! el "5")
    (is= "5" (dommy/value el))
    (dommy/set-value! el "")
    (is= "" (dommy/value el))))

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

(deftest multiple-classes
  (let [el (node [:div.c0])]
    (is (dommy/has-class? el "c0"))
    (doseq [class ["c1" "c2" "c3"]]
      (is (not (dommy/has-class? el class)) (str "el shouldn't have class " class)))

    (dommy/add-class! el "")
    (is (dommy/has-class? el "c0"))
    (doseq [class ["c1" "c2" "c3"]]
      (is (not (dommy/has-class? el class)) (str "el shouldn't have class " class)))

    (dommy/add-class! el " \r\n\t")
    (is (dommy/has-class? el "c0"))
    (doseq [class ["c1" "c2" "c3"]]
      (is (not (dommy/has-class? el class)) (str "el shouldn't have class " class)))

    (dommy/add-class! el "c1 c2 c3")
    (doseq [class ["c0" "c1" "c2" "c3"]]
      (is (dommy/has-class? el class) (str "el should have class " class)))

    (dommy/add-class! el "\t\t    c4 c5 c6\n")
    (doseq [class ["c0" "c1" "c2" "c3" "c4" "c5" "c6"]]
      (is (dommy/has-class? el class) (str "el should have class " class)))))

(deftest variadic-classes
  (let [el (node :div)]
    (dommy/add-class! el :foo :bar)
    (is (dommy/has-class? el :foo))
    (is (dommy/has-class? el :bar))
    (dommy/remove-class! el :foo :bar)
    (is (not (dommy/has-class? el :foo)))
    (is (not (dommy/has-class? el :bar)))
    (dommy/add-class! el "this is" "four classes")
    (is (dommy/has-class? el :this))
    (is (dommy/has-class? el "is"))
    (is (dommy/has-class? el :four))
    (is (dommy/has-class? el "classes"))))

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

(deftest listener-simple
  (let [el-simple (node [:div#id])
        click-cnt (atom 0)]
    (dommy/append! js/document.body el-simple)
    (is= (dommy/listen! el-simple :click (fn [e] #_(js* "debugger") (swap! click-cnt inc)))
         el-simple)
    (is= 0 @click-cnt)
    (dommy/fire! el-simple :click)
    (is= 1 @click-cnt)))

(deftest ancestor-nodes
  (let [parent (node [:.parent [:.child [:.grandchild]]])
        child (sel1 parent :.child)
        grandchild (sel1 parent :.grandchild)]
    (is= [grandchild child parent] (dommy/ancestor-nodes grandchild))
    (is= [parent] (dommy/ancestor-nodes parent))
    (dommy/append! js/document.body parent)
    (is= [parent js/document.body js/document.documentElement js/document]
         (dommy/ancestor-nodes parent))))

(deftest closest-and-matches-pred
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
    (dommy/append! js/document.body el-simple)
    (is= (dommy/listen! el-simple :click (fn [e] #_(js* "debugger") (swap! click-cnt inc)))
         el-simple)
    (is= 0 @click-cnt)
    (dommy/fire! el-simple :click)
    (is= 1 @click-cnt)))

(deftest listen!-multi
  (let [el (node :.test)
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el)
    (is= (dommy/listen! el :click listener :dblclick listener)
         el)
    (is= 0 @click-cnt)
    (dommy/fire! el :click)
    (is= 1 @click-cnt)
    (dommy/fire! el :dblclick)
    (is= 2 @click-cnt)))

(deftest unlisten!
  (let [el-nested (node [:ul [:li "Test"]])
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el-nested)
    (is= (dommy/listen! [el-nested :li] :click listener)
         [el-nested :li])
    (dommy/fire! (sel1 el-nested :li) :click)
    (is= 1 @click-cnt)
    (is= (dommy/unlisten! [el-nested :li] :click listener)
         [el-nested :li])
    (dommy/fire! (sel1 el-nested :li) :click)
    (is= 1 @click-cnt)
    (is= (dommy/listen! [el-nested :li] :click listener)
         [el-nested :li])
    (dommy/fire! (sel1 el-nested :li) :click)
    (is= 2 @click-cnt)
    (let [el-nested (node [:.parent [:.child [:.grandchild]]])]
      (dommy/append! js/document.body el-nested)
      (is= (dommy/listen! [el-nested :.child :.grandchild] :click listener)
           [el-nested :.child :.grandchild])
      (dommy/fire! (sel1 el-nested :.grandchild) :click)
      (is= 3 @click-cnt)
      (is= (dommy/unlisten! [el-nested :.child :.grandchild] :click listener)
           [el-nested :.child :.grandchild])
      (dommy/fire! (sel1 el-nested [:.child :.grandchild]) :click)
      (is= 3 @click-cnt))))

(deftest unlisten!-multi
  (let [el (node :.test)
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el)
    (is= (dommy/listen! el :click listener :dblclick listener)
         el)
    (dommy/fire! el :click)
    (dommy/fire! el :dblclick)
    (is= 2 @click-cnt)
    (is= (dommy/unlisten! el :click listener :dblclick listener)
         el)
    (dommy/fire! el :click)
    (dommy/fire! el :dblclick)
    (is= 2 @click-cnt)))

(deftest listen-once!
  (let [el (node :.test)
        click-cnt (atom 0)]
    (dommy/append! js/document.body el)
    (is= (dommy/listen-once! el :click #(swap! click-cnt inc))
         el)
    (dommy/fire! el :click)
    (is= 1 @click-cnt)
    (dommy/fire! el :click)
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
            (dommy/fire! child evt-type
                   #(doto % (aset "relatedTarget" relatedTarget)))
            (is (some #(= @counter (% orig-count)) [identity inc])
                "counter value is valid (just being defensive)")
            (= @counter (inc orig-count))))
        should-call-listener {"outside" nil
                              "sibling" sibling
                              "parent" parent}
        shouldnt-call-listener {"grandchild" grandchild
                                "greatgrandchild" greatgrandchild}]
    (dommy/append! js/document.body parent)
    (doseq [[fake-evt real-evt] {:mouseenter :mouseover, :mouseleave :mouseout}
            elem-sel [child [parent :.child]]]
      (dommy/listen! elem-sel fake-evt listener)
      (doseq [[where relatedTarget] should-call-listener]
        (is (fire!-called-listener? real-evt relatedTarget)
            (format "%s to/from %s is %s" (name real-evt) where (name fake-evt))))
      (doseq [[where relatedTarget] shouldnt-call-listener]
        (is (not (fire!-called-listener? real-evt relatedTarget))
            (format "%s to/from %s isn't %s" (name real-evt) where (name fake-evt))))
      (dommy/unlisten! elem-sel fake-evt listener)
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

(deftest set-style!
  (let [el (node [:div])]
    (dommy/set-style! el :height "0px" :width "1px" :z-index 1)
    (is= (-> el .-style .-height) "0px")
    (is= (-> el .-style .-width) "1px")
    (is= (-> el .-style .-zIndex) "1")))

(deftest style
  (let [el (node [:div])]
    (dommy/prepend! js/document.body el)
    (dommy/set-style! el :height "0px" :orphans 666)
    (is= (dommy/style el :height) "0px")
    (is= (dommy/style el :orphans) "666")))

(deftest set-px!
  (let [el (node [:div])]
    (dommy/set-px! el :height 0 :width 1)
    (is= (-> el .-style .-height) "0px")
    (is= (-> el .-style .-width) "1px")))

(deftest px
  (let [el (node [:div])]
    (dommy/prepend! js/document.body el)
    (dommy/set-style! el :height "0px" :width "1px")
    (is= (dommy/px el :height) 0)
    (is= (dommy/px el :width) 1)))

(deftest ->Array
  (let [array (utils/->Array (js* "{length: 2, 0: 'lol', 1: 'wut'}"))]
    (is (instance? js/Array array))
    (is= (.-length array) 2)
    (is= (aget array 0) "lol")
    (is= (aget array 1) "wut")))

(deftest bounding-client-rect
  (set! js/document.body.scrollTop 42)
  (let [el (doto (node [:div {:style {:position "absolute" :left "100px" :top "200px"}}])
             (->> (dommy/append! js/document.body)))
        {:keys [left top]} (dommy/bounding-client-rect el)]
    (is= left 100)
    (is= top 158)))

;; Performance test to run in browser

(defn class-perf-test [elem]
  (let [start (.now js/Date)]
    (dotimes [i 1e6]
      (dommy/has-class? elem (str "class" (inc (mod i 10))))
      (dommy/toggle-class! elem (str "class" (inc (mod i 10)))))
    (/ (- (.now js/Date) start) 1000)))

(defn jquery-perf-test [elem]
  (let [elem (js/jQuery elem)
        start (.now js/Date)]
    (dotimes [i 1e6]
      (.hasClass elem (str "class" (inc (mod i 10))))
      (.toggleClass elem (str "class" (inc (mod i 10)))))
    (/ (- (.now js/Date) start) 1000)))

(defn ^:export class-perf []
  (let [elem (node [:div.class1.class2.class3.class4.class5])]
    (.log js/console (pr-str {:dommy  (class-perf-test elem)
                              :jquery (jquery-perf-test elem)}))))
