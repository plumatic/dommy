(ns dommy.macros
  (:require [clojure.string :as str]))

(declare node)

(defn constant? [data]
  (some #(% data) [number? keyword? string?]))

(defn all-constant? [data]
  (cond
   (coll? data) (every? all-constant? data)
   (constant? data) true))

(defn single-selector? [data]
  (and (constant? data)
       (re-matches #"^\S+$" (name data))))

(defn id-selector? [s]
  (and (constant? s)
       (re-matches #"^#\w+$" (name s))))

(defn class-selector? [s]
  (and (constant? s)
       (re-matches #"^\.\w+$" (name s))))

(defn tag-selector? [s]
  (and (constant? s)
       (re-matches #"^\w+$" (name s))))

(defn selector [data]
  (cond
   (coll? data) (str/join " " (map selector data))
   (constant? data) (name data)))

(defn selector-form [data]
  (if (constant? data)
    (selector data)
    `(dommy.core/selector ~data)))

(defmacro by-id [id]
  (let [id (-> id name (str/replace #"#" ""))]
    `(js/document.getElementById ~id)))

(defmacro by-class
  ([base data]
     (let [data (-> data name (str/replace "." ""))]
       `(dommy.core/->Array
         (.getElementsByClassName ~base ~data))))
  ([data]
     `(by-class js/document ~data)))

(defmacro by-tag
  ([base data]
     `(dommy.core/->Array
       (.getElementsByTagName ~base ~(name data))))
  ([data]
     `(by-tag js/document ~data)))

(defmacro sel1
  ([base data]
     (if (single-selector? data)
       (condp #(%1 %2) (name data)
         #(identical? "body" %) `js/document.body
         #(identical? "head" %) `js/document.head
         id-selector? `(by-id ~data)
         class-selector? `(aget (by-class ~base ~data) 0)
         tag-selector? `(aget (by-tag ~base ~data) 0))
       `(.querySelector (node ~base) ~(selector-form data))))
  ([data]
     `(sel1 js/document ~data)))

(defmacro sel
  ([base data]
     (if (single-selector? data)
       (condp #(%1 %2) (name data)
         class-selector? `(by-class ~base ~data)
         tag-selector? `(by-tag ~base ~data))
       `(dommy.core/->Array
         (.querySelectorAll (node ~base) ~(selector-form data)))))
  ([data]
     `(sel js/document ~data)))

(defmacro compile-add-attr!
  "compile-time add attribute"
  [d k v]
  (assert (keyword? k))
  `(when ~v
     ~(cond
       (identical? k :class) `(set! (.-className ~d) (.trim (str (.-className ~d) " " ~v)))
       (identical? k :style) `(.setAttribute ~d ~(name k) (dommy.core/style-str ~v))
       (identical? k :classes) `(compile-add-attr! ~d :class ~(str/join " " (map name v)))
       :else `(.setAttribute ~d ~(name k) ~v))))

(defn parse-keyword
  "return pair [tag class-str id] where tag is dom tag and attrs
   are key-value attribute pairs from css-style dom selector"
  [node-key]
  (let [node-str (name node-key)
        node-tag (second (re-find #"^([^.\#]+)[.\#]?" node-str))
        classes (map #(.substring ^String % 1) (re-seq #"\.[^.*]*" node-str))
        id (first (map #(.substring ^String % 1) (re-seq #"#[^.*]*" node-str)))]
    [(if (empty? node-tag) "div" node-tag)
     (str/join " " classes)
     id]))

(defmacro compile-compound [[node-key & rest]]
  (let [literal-attrs (when (map? (first rest)) (first rest))
        var-attrs (when (and (not literal-attrs) (-> rest first meta :attrs))
                    (first rest))
        children (drop (if (or literal-attrs var-attrs) 1 0) rest)
        [tag class-str id] (parse-keyword node-key)
        dom-sym (gensym "dom")]
    `(let [~dom-sym (.createElement js/document ~(name tag))]
       ~@(when-not (empty? class-str)
           [`(set! (.-className ~dom-sym) ~class-str)])
       ~@(when id
           [`(.setAttribute ~dom-sym "id" ~id)])
       ~@(for [[k v] literal-attrs]
           (if (keyword? k)
             `(compile-add-attr! ~dom-sym ~k ~v)
             `(dommy.core/set-attr! ~dom-sym ~k ~v)))
       ~@(when var-attrs
           [`(doseq [[k# v#] ~var-attrs]
               (dommy.core/set-attr! ~dom-sym k# v#))])
       ~@(for [c children]
           `(.appendChild ~dom-sym (node ~c)))
       ~dom-sym)))

(defmacro node [data]
  (cond
   (= (str data) "js/document") `js/document
   (vector? data) `(compile-compound ~data)
   (keyword? data) `(compile-compound [~data])
   (or (string? data) (:text (meta data))) `(.createTextNode js/document ~data)
   :else `(dommy.template/->node-like ~data)))

(defmacro deftemplate [name args & node-forms]
  `(defn ~name ~args
     ~(if (next node-forms)
        (let [doc-frag (gensym "frag")]
          `(let [~doc-frag (.createDocumentFragment js/document)]
             ~@(for [el node-forms]
                 `(.appendChild ~doc-frag (node ~el)))
             ~doc-frag))
        `(node ~(first node-forms)))))
