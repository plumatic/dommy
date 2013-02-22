(ns dommy.template-perf-test
  (:use-macros
   [dommy.macros :only [node]])
  (:require
   [dommy.template :as template]
   [crate.core :as crate]))

;; Perf Test: dommy vs. crate. vs. jQuery

(defn dommy-template [datum]
  (template/node
   [:li [:a {:href (str "#show/" (:key datum))}
         [:div.class1.class2 {:id (str "item" (:key datum))}
          [:span.anchor (:name datum)]]]]))


(defn dommy-compiled [datum]
  (node
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
  (-> "<li>" js/jQuery 
     (.append
      (-> "<a>" js/jQuery 
          (.attr "href" (str "#show/" (:key datum)))
          (.addClass "anchor")
          (.append (-> "<div>" js/jQuery
                       (.addClass "class1")
                       (.addClass "class2")
                       (.attr "id" (str "item" (:key datum)))
                       (.append (-> "<span>" js/jQuery (.text (:name datum))))))))))

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
              [:dommy dommy-template]
              [:dommy-compiled dommy-compiled]])]    
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
     (pr-str (->> (for [i (range 3)]
                     (into {} (time-test data)))
                  (reduce (partial merge-with +))
                  (map (fn [[k v]] [k (/ v 3)]))
                  (into {}))))))