(ns dommy.template-test
  (:require [dommy.template :as template])
  (:require-macros [dommy.template-compile :as template-compile])
  (:use-macros [dommy.template-compile :only [deftemplate]]
               [cljs-test.core :only [is is= deftest]]))


(deftest simple-template 
  (is= "B" (-> :b template/node .-tagName))
  (is= "some text" (-> "some text" template/node .-textContent))
  ;; unfortunately to satisfy the macro gods, you need to
  ;; duplicate the vector literal to test compiled and runtime template
  (let [e1 (template/node [:span "some text"])
        e2 (template-compile/node [:span "some text"])]
    (doseq [e [e1 e2]]
      (is= "SPAN" (.-tagName e))
      (is= "some text" (.-textContent e))
      (is= js/document.TEXT_NODE (-> e .-childNodes (aget 0) .-nodeType))
      (is (zero? (-> e .-children .-length)))))
  (let [e1 (template/node [:a {:classes ["class1" "class2"] :href "http://somelink"} "anchor"])
        e2 (template-compile/node
              [:a {:classes ["class1" "class2"] :href "http://somelink"} "anchor"])]
    (doseq [e [e1 e2]] (is (-> e .-tagName (= "A")))
           (is= "anchor" (.-textContent e))
           (is= "http://somelink" (.getAttribute e "href"))
           (is= "class1 class2" (.-className e))))
  (let [e1 (template/base-element :div#id.class1.class2)
        e2 (template-compile/node :div#id.class1.class2)]
    (doseq [e [e1 e2]]
      (is= "DIV" (.-tagName e))
      (is= "id" (.getAttribute e "id"))
      (is= "class1 class2" (.-className e))))
  (let [e1 (template/compound-element [:div {:style {:margin-left "15px"}}])
        e2 (template-compile/node [:div {:style {:margin-left "15px"}}])]
    (doseq [e [e1 e2]]
      (is= "DIV" (.-tagName e))
      (is= "margin-left:15px;" (.getAttribute e "style"))))
  (let [e (template/compound-element [:div (interpose [:br] (repeat 3 "test"))])]
    (is (-> e .-outerHTML (= "<div>test<br>test<br>test</div>"))))
  (let [e1 (template/compound-element [:div.class1 [:span#id1 "span1"] [:span#id2 "span2"]])
        e2 (template-compile/node [:div.class1 [:span#id1 "span1"] [:span#id2 "span2"]])]
    (doseq [e [e1 e2]]
      (is= "span1span2" (.-textContent e))
      (is= "class1" (.-className e))
      (is= 2 (-> e .-childNodes .-length))
      (is="<span id=\"id1\">span1</span><span id=\"id2\">span2</span>"
          (.-innerHTML e))
      (is= "span1" (-> e .-childNodes (aget 0) .-innerHTML))
      (is= "span2" (-> e .-childNodes (aget 1) .-innerHTML))))
  (is= "<span id=\"id1\">span1</span><span id=\"id2\">span2</span>"
       (-> [:div (for [x [1 2]] [:span {:id (str "id" x)} (str "span" x)])]
           template/node
           .-innerHTML))
  (let [e (first (template/html->nodes "<div><p>some-text</p></div>"))]
    (is= "DIV" ( .-tagName e))
    (is= "<p>some-text</p>" (.-innerHTML e))
    (is=  e (template/node e)))
  (let [e1 (template/base-element :#id1.class1)
        e2 (template-compile/node :#id1.class1)]
    (doseq [e [e1 e2]]      
      (is= (.-outerHTML (template/base-element :div#id1.class1))
           (.-outerHTML (template/base-element :#id1.class1))))))

(deftest nested-template-test
   ;; test html for example list form
  ;; note: if practice you can write the direct form (without the list) you should.
  (let [spans (for [i (range 2)] [:span (str "span" i)])
        end [:span.end "end"]
        h   [:div#id1.class1 (list spans end)]
        e1 (template/compound-element h)
        e2 (template/node             h)]
    (doseq [e [e1 e2]]
      (is (-> e .-textContent (= "span0span1end")))
      (is (-> e .-className (= "class1")))
      (is (-> e .-childNodes .-length (= 3)))
      (is (-> e .-innerHTML 
                (= "<span>span0</span><span>span1</span><span class=\"end\">end</span>")))
      (is (-> e .-childNodes (aget 0) .-innerHTML (= "span0")))
      (is (-> e .-childNodes (aget 1) .-innerHTML (= "span1")))
      (is (-> e .-childNodes (aget 2) .-innerHTML (= "end")))))
  
  ;; test equivalence of "direct inline" and list forms
  (let [spans (for [i (range 2)] [:span (str "span" i)])
        end   [:span.end "end"]
        h1    [:div.class1 (list spans end)]
        h2    [:div.class1 spans end]
        e11 (template/compound-element h1)
        e12 (template/node             h1)
        e21 (template/compound-element h2)
        e22 (template/node             h2)]
    (doseq [[e1 e2] [[e11 e12]
                     [e12 e21]
                     [e21 e22]
                     [e22 e11]]]
      (is (= (.-innerHTML e1) (.-innerHTML e2))))))

(deftest boolean 
  (let [e1 (template/node [:option {:selected true} "some text"])
        e1c (template-compile/node [:option {:selected true} "some text"]) 
        e2 (template/node [:option {:selected false} "some text"])
        e2c (template-compile/node [:option {:selected false} "some text"])
        e3 (template/node [:option {:selected nil} "some text"])
        e3c (template-compile/node [:option {:selected nil} "some text"])]
    (doseq [e [e1 e1c]] (is (-> e (.getAttribute "selected") (= "true"))))
    (doseq [e [e2 e2c]] (is (-> e (.getAttribute "selected") (nil?))))
    (doseq [e [e3 e3c]] (is (-> e (.getAttribute "selected") (nil?))))))


(deftemplate simple-template [[href anchor]]
    [:a.anchor {:href href} ^:text anchor])

(deftest  deftemplate
  (is= "<a class=\"anchor\" href=\"http://somelink.html\">some-text</a>"
       (.-outerHTML (simple-template ["http://somelink.html" "some-text"]))))


(deftemplate nested-template [n]
  [:ul.class1 (for [i (range n)] [:li i])])

(deftest nested-deftemplate
  (is= "<ul class=\"class1\"><li>0</li><li>1</li><li>2</li><li>3</li><li>4</li></ul>"
       (.-outerHTML (nested-template 5))))

(deftemplate span-wrapper [content]
  [:span content])

(deftest empty-string-in-template
  (is= "<span></span>"
       (.-outerHTML (span-wrapper ""))))
