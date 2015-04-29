(ns dommy.core-test
  (:refer-clojure :exclude [ancestors])
  (:require-macros
   [cemerick.cljs.test :refer [is deftest testing]]
   [dommy.core :refer [sel sel1]])
  (:require
   [cemerick.cljs.test :as test]
   [dommy.test-utils :refer [ce el-tree fire!]]
   [dommy.utils :as utils]
   [dommy.core :as dommy]))

(deftest set-html!
  (let [el (ce :span)]
    (is (= "" (dommy/html el)))
    (dommy/set-html! el "hello <i>world</i>")
    (is (= "hello <i>world</i>" (dommy/html el)))
    (dommy/set-html! el "")
    (is (= "" (dommy/html el)))))

(deftest text
  (let [el (-> (ce :span "foo")
               (dommy/append! (ce :b "bar")))]
    (is (= "foobar" (dommy/text el)))
    (dommy/set-style! el :display "none")
    (is (= "foobar" (dommy/text el)))))

(deftest set-text!
  (let [el (ce :span)]
    (is (= "" (dommy/text el)))
    (dommy/set-text! el "test")
    (is (= "test" (dommy/text el)))
    (dommy/set-text! el "<i>woah</i>")
    (is (= "<i>woah</i>" (dommy/text el)))
    (dommy/set-text! el "")
    (is (= "" (dommy/text el)))))

(deftest value
  (let [el (-> (ce :input))]
    (is (= "" (dommy/value el)))))

(deftest set-value!
  (let [el (ce :input)]
    (is (= "" (dommy/value el)))
    (dommy/set-value! el "test")
    (is (= "test" (dommy/value el)))))

(deftest append!
  (let [container (ce :div)
        el (ce :span "test")]
    (dommy/append! container el)
    (is (= 1 (.-childElementCount container)))
    (is (= el (.-lastChild container)))))

(deftest prepend!
  (let [container (ce :div)
        el (ce :span "test")]
    (dommy/prepend! container el)
    (is (= 1 (.-childElementCount container)))
    (is (= el (.-firstChild container)))))

(deftest insert-before!
  (let [container (ce :div)
        placemark (ce :span "placemark")
        el (ce :span "test")]
    (dommy/append! container placemark)
    (let [res (dommy/insert-before! el placemark)]
      (is (= 2 (.-childElementCount container)))
      (is (= el (.-firstChild container)))
      (is (= res el)))))

(deftest insert-after!
  (let [container (ce :div)
        placemark (ce :span "placemark")
        el (ce :span "test")]
    (dommy/prepend! container placemark)
    (let [res (dommy/insert-after! el placemark)]
      (is (= 2 (.-childElementCount container)))
      (is (= el (.-lastChild container)))
      (is (= res el)))))

(deftest basic-selection
  (let [el (-> (ce :div) (dommy/set-attr! :id "test"))]
    (dommy/append! js/document.body el)
    (is (= (sel1 :#test) el))))

(deftest simple-class
  (let [el (ce :div)]
    (is (not (dommy/has-class? el "test")))

    (dommy/add-class! el "test")
    (is (dommy/has-class? el "test"))

    (dommy/remove-class! el "test")
    (is (not (dommy/has-class? el "test")))

    (dommy/toggle-class! el "test")
    (is (dommy/has-class? el "test"))))

(deftest multiple-classes
  (let [el (ce :div)]
    (dommy/add-class! el "c1 c2 c3")
    (doseq [class ["c1" "c2" "c3"]]
      (is (dommy/has-class? el class)
          (str "el should have class " class)))

    (dommy/add-class! el "\t\t    c4 c5 c6\n")
    (doseq [class ["c1" "c2" "c3" "c4" "c5" "c6"]]
      (is (dommy/has-class? el class)
          (str "el should have class " class)))))

(deftest variadic-classes
  (let [el (ce :div)]
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
  (let [grandchild (ce :div)
        child (-> (ce :div) (dommy/append! grandchild))
        sibling (ce :div)
        parent (-> (ce :div) (dommy/append! child sibling))]
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
  (let [el (ce :div)
        click-cnt (atom 0)]
    (dommy/append! js/document.body el)
    (is (= (dommy/listen! el :click (fn [e] (swap! click-cnt inc)))
           el))
    (is (= 0 @click-cnt))
    (fire! el :click)
    (is (= 1 @click-cnt))))

(deftest ancestors
  (let [[grandchild child parent] (el-tree)]
    (is (= [grandchild child parent] (dommy/ancestors grandchild)))
    (is (= [parent] (dommy/ancestors parent)))
    (dommy/append! js/document.body parent)
    (is (= [parent js/document.body js/document.documentElement js/document]
           (dommy/ancestors parent)))))

(deftest closest-and-matches-pred
  (let [[grandchild child parent] (el-tree)]
    (is (= grandchild (dommy/closest parent grandchild :.grandchild)))
    (is (= child (dommy/closest parent grandchild :.child)))
    (is (nil? (dommy/closest parent grandchild :.parent)))
    (dommy/append! js/document.body parent)
    (is (= parent (dommy/closest grandchild :.parent)))))

(deftest live-listener
  (let [[grandchild child parent] (el-tree)
        click-cnt (atom 0)
        fake-event (js-obj "target" grandchild)]
    ((dommy/live-listener
      parent :.grandchild
      (fn [event]
        (swap! click-cnt inc)
        (is (= grandchild (.-selectedTarget event)))))
     fake-event)
    (is (= 1 @click-cnt))
    ((dommy/live-listener
      parent :.child
      (fn [event]
        (swap! click-cnt inc)
        (is (= child (.-selectedTarget event)))))
     fake-event)
    (is (= 2 @click-cnt))
    ((dommy/live-listener
      parent :.parent
      #(swap! click-cnt inc))
     fake-event)
    (is (= 2 @click-cnt))))

(deftest window-listen!
  (let [click-cnt (atom 0)
        handler #(swap! click-cnt inc)]
    (is (= (dommy/listen! js/window :click handler)
           js/window))
    (is (= 0 @click-cnt))
    (fire! js/window :click)
    (is (= 1 @click-cnt))
    (dommy/unlisten! js/window :click handler)
    (fire! js/window :click)
    (is (= 1 @click-cnt))))

(deftest listen!
  (let [el (ce :div)
        click-cnt (atom 0)]
    (dommy/append! js/document.body el)
    (is (= (dommy/listen! el :click (fn [e] (swap! click-cnt inc)))
           el))
    (is (= 0 @click-cnt))
    (fire! el :click)
    (is (= 1 @click-cnt))))

(deftest listen!-multi
  (let [el (ce :div)
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el)
    (is (= (dommy/listen! el :click listener :dblclick listener)
           el))
    (is (= 0 @click-cnt))
    (fire! el :click)
    (is (= 1 @click-cnt))
    (fire! el :dblclick)
    (is (= 2 @click-cnt))))

(deftest unlisten!
  (let [el-nested (-> (ce :ul) (dommy/append! (ce :li "Test")))
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el-nested)
    (is (= (dommy/listen! [el-nested :li] :click listener)
           [el-nested :li]))
    (fire! (sel1 el-nested :li) :click)
    (is (= 1 @click-cnt))
    (is (= (dommy/unlisten! [el-nested :li] :click listener)
           [el-nested :li]))
    (fire! (sel1 el-nested :li) :click)
    (is (= 1 @click-cnt))
    (is (= (dommy/listen! [el-nested :li] :click listener)
           [el-nested :li]))
    (fire! (sel1 el-nested :li) :click)
    (is (= 2 @click-cnt))
    (let [[_ _ el-nested] (el-tree)]
      (dommy/append! js/document.body el-nested)
      (is (= (dommy/listen! [el-nested :.child :.grandchild] :click listener)
             [el-nested :.child :.grandchild]))
      (fire! (sel1 el-nested :.grandchild) :click)
      (is (= 3 @click-cnt))
      (is (= (dommy/unlisten! [el-nested :.child :.grandchild] :click listener)
             [el-nested :.child :.grandchild]))
      (fire! (sel1 el-nested [:.child :.grandchild]) :click)
      (is (= 3 @click-cnt)))))

(deftest unlisten!-multi
  (let [el (ce :div)
        click-cnt (atom 0)
        listener #(swap! click-cnt inc)]
    (dommy/append! js/document.body el)
    (is (= (dommy/listen! el :click listener :dblclick listener)
           el))
    (fire! el :click)
    (fire! el :dblclick)
    (is (= 2 @click-cnt))
    (is (= (dommy/unlisten! el :click listener :dblclick listener)
           el))
    (fire! el :click)
    (fire! el :dblclick)
    (is (= 2 @click-cnt))))

(deftest listen-once!
  (let [el (ce :div)
        click-cnt (atom 0)]
    (dommy/append! js/document.body el)
    (is (= (dommy/listen-once! el :click #(swap! click-cnt inc))
           el))
    (fire! el :click)
    (is (= 1 @click-cnt))
    (fire! el :click)
    (is (= 1 @click-cnt))))

(deftest toggle!
  (let [el (ce :div)]
    (is (not (dommy/hidden? el)))
    (dommy/toggle! el)
    (is (= "none" (-> el .-style .-display)))
    (dommy/toggle! el false)
    (is (= "none" (-> el .-style .-display)))
    (dommy/toggle! el true)
    (is (not (dommy/hidden? el)))))

(deftest set-style!
  (let [el (ce :div)]
    (dommy/set-style! el :height "0px" :width "1px" :z-index 1)
    (is (= (-> el .-style .-height) "0px"))
    (is (= (-> el .-style .-width) "1px"))
    (is (= (-> el .-style .-zIndex) "1"))))

(deftest style
  (let [el (ce :div)]
    (dommy/prepend! js/document.body el)
    (dommy/set-style! el :height "0px" :orphans 666)
    (is (= (dommy/style el :height) "0px"))
    (is (= (dommy/style el :orphans) "666"))
    (dommy/remove-style! el :height :orphans)
    (is (empty? (-> el .-style .-height)))
    (is (empty? (-> el .-style .-orphans)))))

(deftest set-px!
  (let [el (ce :div)]
    (dommy/set-px! el :height 0 :width 1)
    (is (= (-> el .-style .-height) "0px"))
    (is (= (-> el .-style .-width) "1px"))))

(deftest px
  (let [el (ce :div)]
    (dommy/prepend! js/document.body el)
    (dommy/set-style! el :height "0px" :width "1px")
    (is (= (dommy/px el :height) 0))
    (is (= (dommy/px el :width) 1))))

(deftest bounding-client-rect
  (let [el (doto (ce :div)
             (dommy/set-style!
              :position "absolute"
              :left "100px"
              :top "200px")
             (->> (dommy/append! js/document.body)))
        {:keys [left top]} (dommy/bounding-client-rect el)]
    (is (= left 100))
    (is (= top 200))))

(deftest ->Array
  (let [array (utils/->Array (js* "{length: 2, 0: 'lol', 1: 'wut'}"))]
    (is (instance? js/Array array))
    (is (= (.-length array) 2))
    (is (= (aget array 0) "lol"))
    (is (= (aget array 1) "wut"))))

(deftest set-attr!
  (let [el (ce :input)]
    (dommy/set-attr! el :disabled)
    (is (= (dommy/attr el :disabled) "disabled"))))