(ns dommy.core
  (:require
   [clojure.string :as str]
   [dommy.utils :as utils]))

(defmacro by-id
  "Returns the DOM node with id in document if it exists, otherwise nil.
   `id` can be a string or keyword. Expands to `document.getElementById`."
  [id]
  (let [id (-> id utils/as-str (str/replace #"#" ""))]
    `(js/document.getElementById ~id)))

(defmacro by-class
  "Returns a sequence of DOM nodes selected by `class`. `class` can be
   a string or keyword and should not include the . selector prefix.
   Expands to `base.getElementsByClassName`. If `base` node is given,
   selection is limited to its descendant nodes."
  ([class] `(by-class js/document ~class))
  ([base class]
     (let [class (-> class utils/as-str (str/replace "." ""))]
       `(dommy.utils/->Array
         (.getElementsByClassName ~base ~class)))))

(defmacro by-tag
  "Same behavior as `by-class`, selecting by `tag` instead of a class."
  ([tag] `(by-tag js/document ~tag))
  ([base tag]
     `(dommy.utils/->Array
       (.getElementsByTagName ~base ~(utils/as-str tag)))))

(defmacro sel1
  "Select a single DOM node. Tries to expand to the native selector methods
   as defined by `by-id`, `by-class`, and `by-tag`, falling back to
   `document.querySelector` when the selector isn't a constant."
  ([data] `(sel1 js/document ~data))
  ([base data]
     (if (utils/constant? data)
       (condp #(%1 %2) (utils/as-str data)
         #(= "body" %) `js/document.body
         #(= "head" %) `js/document.head
         #(and (= 'js/document base) (utils/id-selector? %)) `(by-id ~data)
         utils/class-selector? `(aget (by-class ~base ~data) 0)
         utils/tag-selector? `(aget (by-tag ~base ~data) 0)
         (utils/query-selector base data))
       (utils/query-selector base data))))

(defmacro sel
  "Same behavior as `sel1`, instead keeping the full array that `by-class`
   or `by-tag` expand to. Falls back to `document.querySelectorAll` instead of
   `document.querySelector`."
  ([data] `(sel js/document ~data))
  ([base data]
     (if (utils/constant? data)
       (condp #(%1 %2) (utils/as-str data)
         utils/class-selector? `(by-class ~base ~data)
         utils/tag-selector? `(by-tag ~base ~data)
         (utils/query-selector-all base data))
       (utils/query-selector-all base data))))
