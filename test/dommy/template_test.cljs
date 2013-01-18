(ns dommy.template-test
  (:require [dommy.template :as template]))

(defn ^:export simple-test []
  (assert (-> :b template/node .-tagName (= "B")))
  (assert (-> "some text" template/node .-textContent (= "some text")))
  (let [e (template/node [:span "some text"])]
    (assert (-> e .-tagName (= "SPAN")))
    (assert (-> e .-textContent (= "some text")))
    (assert (-> e .-childNodes (aget 0) .-nodeType (= js/document.TEXT_NODE)))
    (assert (-> e .-children .-length zero?)))
  (let [e (template/node [:a {:classes ["class1" "class2"] :href "http://somelink"} "anchor"])]
    (assert (-> e .-tagName (= "A")))
    (assert (-> e .-textContent (= "anchor")))
    (assert (-> e (.getAttribute "href") (= "http://somelink")))
    (assert (-> e .-className (= "class1 class2"))))
  (let [e (template/base-element :div#id.class1.class2)]
    (assert (-> e .-tagName (= "DIV")))
    (assert (-> e (.getAttribute "id") (= "id")))
    (assert (-> e .-className (= "class1 class2"))))
  (let [e (template/compound-element [:div {:style {:margin-left "15px"}}])]
    (assert (-> e .-tagName (= "DIV")))
    (assert (-> e (.getAttribute "style") (= "margin-left:15px;"))))
  (let [e (template/compound-element [:div.class1 [:span#id1 "span1"] [:span#id2 "span2"]])]
    (assert (-> e .-textContent (= "span1span2")))
    (assert (-> e .-className (= "class1")))
    (assert (-> e .-childNodes .-length (= 2)))
    (assert (-> e .-innerHTML (= "<span id=\"id1\">span1</span><span id=\"id2\">span2</span>")))
    (assert (-> e .-childNodes (aget 0) .-innerHTML (= "span1")))
    (assert (-> e .-childNodes (aget 1) .-innerHTML (= "span2"))))
  (assert (= "<span id=\"id1\">span1</span><span id=\"id2\">span2</span>"
             (-> [:div (for [x [1 2]] [:span {:id (str "id" x)} (str "span" x)])]
                 template/node
                 .-innerHTML)))
  (let [e (first (template/html->nodes "<div><p>some-text</p></div>"))]
    (assert (-> e .-tagName (= "DIV")))
    (assert (-> e .-innerHTML (= "<p>some-text</p>")))
    (assert (= e (template/node e))))
  (let [e (template/base-element :#id1.class1)]
    (assert (=  (.-outerHTML (template/base-element :div#id1.class1))
                (.-outerHTML (template/base-element :#id1.class1)))))
  (.log js/console "PASS simple-test"))

(simple-test)
