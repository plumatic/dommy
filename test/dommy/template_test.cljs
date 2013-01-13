(ns dommy.template-test
  (:require [dommy.template :as template]
            [crate.core :as crate]))

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
    (assert (-> e (.getAttribute "class") (= "class1 class2"))))
  (let [e (template/base-element :div#id.class1.class2)]
    (assert (-> e .-tagName (= "DIV")))
    (assert (-> e (.getAttribute "id") (= "id")))
    (assert (-> e (.getAttribute "class") (= "class1 class2"))))
  (let [e (template/compound-element [:div {:style {:margin-left "15px"}}])]
    (assert (-> e .-tagName (= "DIV")))
    (assert (-> e (.getAttribute "style") (= "margin-left:15px;"))))
  (let [e (template/compound-element [:div.class1 [:span#id1 "span1"] [:span#id2 "span2"]])]
    (assert (-> e .-textContent (= "span1span2")))
    (assert (-> e (.getAttribute "class") (= "class1")))
    (assert (-> e .-childNodes .-length (= 2)))
    (assert (-> e .-innerHTML (= "<span id=\"id1\">span1</span><span id=\"id2\">span2</span>")))
    (assert (-> e .-childNodes (aget 0) .-innerHTML (= "span1")))
    (assert (-> e .-childNodes (aget 1) .-innerHTML (= "span2"))))
  (assert (= "<span id=\"id1\">span1</span><span id=\"id2\">span2</span>"
             (-> [:div (for [x [1 2]] [:span {:id (str "id" x)} (str "span" x)])]
                 template/node
                 .-innerHTML)))
  (.log js/console "PASS simple-test"))

;; Perf Test: dommy vs. crate. vs. jQuery

(defn dommy-template [datum]
  (template/node
   [:li [:a {:href (str "#show/" (:key datum))}
         [:div.class1.class2 {:id (str "item" (:key datum))}
          [:span.anchor (:name datum)]]]]))

(defn crate-template [datum]
  (crate/html
   [:li [:a {:href (str "#show/" (:key datum))}]
        [:div {:id (str "item" (:key datum))
               :class ["class1" "class2"]}
         [:span {:class "anchor"} (:name datum)]]]))

(defn jquery-template [datum]
  (-> "<a>" js/jQuery 
      (.attr "href" (str "#show/" (:key datum)))
      (.addClass "anchor")
      (.append (-> "<div>" js/jQuery
                   (.addClass "class1")
                   (.addClass "class2")
                   (.attr "id" (str "item" (:key datum)))
                   (.append (-> "<span>" js/jQuery (.text (:name datum))))))))

(defn run-test [root data li-fn]
  (let [now (js/Date.)]
    (doseq [d data]
      (.append root (li-fn d)))
    (/ (- (js/Date.) now) 1000)))

(defn gen-data []
  (for [i (range 1e4)]
    {:key (rand-int 1e6)
     :name (str "product" i)}))

(defn time-test [data]
  (for [[key li-fn]
            (shuffle
             [[:jquery jquery-template]
              [:crate crate-template]
              [:dommy dommy-template]])]    
      (let [ul  (-> "<ul>" js/jQuery (.addClass "products"))
            secs (run-test ul data li-fn)]
        [key secs])))

(defn ^:export dommy-profile []
  (let [data (doall (gen-data))]
    (dotimes [_ 3]
      (let [ul  (-> "<ul>" js/jQuery (.addClass "products"))]
        (run-test ul data dommy-template))
      (.log js/console "DONE"))))

(defn ^:export perf-test []
  (let [data (doall (gen-data))]
    (.log js/console
     (pr-str (->> (for [i (range 6)]
                     (into {} (time-test data)))
                  (drop 3)
                  (reduce (partial merge-with +))
                  (map (fn [[k v]] [k (/ v 3)]))
                  (into {}))))))