(ns dommy.core
  (:require
   [clojure.string :as str]
   [dommy.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selectors

(defmacro by-id [id]
  (let [id (-> id utils/as-str (str/replace #"#" ""))]
    `(js/document.getElementById ~id)))

(defmacro by-class
  ([base data]
     (let [data (-> data utils/as-str (str/replace "." ""))]
       `(dommy.utils/->Array
         (.getElementsByClassName ~base ~data))))
  ([data]
     `(by-class js/document ~data)))

(defmacro by-tag
  ([base data]
     `(dommy.utils/->Array
       (.getElementsByTagName ~base ~(utils/as-str data))))
  ([data]
     `(by-tag js/document ~data)))

(defmacro sel1
  ([base data]
     (if (utils/constant? data)
       (condp #(%1 %2) (utils/as-str data)
         #(= "body" %) `js/document.body
         #(= "head" %) `js/document.head
         #(and (= 'js/document base) (utils/id-selector? %)) `(by-id ~data)
         utils/class-selector? `(aget (by-class ~base ~data) 0)
         utils/tag-selector? `(aget (by-tag ~base ~data) 0)
         (utils/query-selector base data))
       (utils/query-selector base data)))
  ([data]
     `(sel1 js/document ~data)))

(defmacro sel
  ([base data]
     (if (utils/constant? data)
       (condp #(%1 %2) (utils/as-str data)
         utils/class-selector? `(by-class ~base ~data)
         utils/tag-selector? `(by-tag ~base ~data)
         (utils/query-selector-all base data))
       (utils/query-selector-all base data)))
  ([data]
     `(sel js/document ~data)))
