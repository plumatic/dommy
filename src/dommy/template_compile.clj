(ns dommy.template-compile
  "Clojure macros to generate procedural DOM building when
   passed nested structured vectors at compile time. Much faster than runtime templating"
  (:require [clojure.string :as str]))

(defmacro compile-add-attr! 
  "compile-time add attribute"
  [d k v]  
  (assert (keyword? k))  
  `(when ~v
     ~(cond
       (identical? k :class) `(set! (.-className ~d) (.trim (str (.-className ~d) " " ~v)))
       (identical? k :style) `(.setAttribute ~d ~(name k) (dommy.template/style-str ~v))
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

(declare node)

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
             `(dommy.template/add-attr! ~dom-sym ~k ~v)))
       ~@(when var-attrs
           [`(doseq [[k# v#] ~var-attrs]
               (dommy.template/add-attr! ~dom-sym k# v#))])
       ~@(for [c children]
           `(.appendChild ~dom-sym (node ~c))) 
       ~dom-sym)))

(defmacro node [data]
  (cond 
    (vector? data) `(compile-compound ~data)
    (keyword? data) `(compile-compound [~data])
    (or (string? data) (:text (meta data))) `(.createTextNode js/document ~data)
    :else `(dommy.template/-elem ~data)))

(defmacro deftemplate [name args node-form]
  `(defn ~name ~args
     (node ~node-form)))

(comment
   
  (compile-compound
   [:a {:classes ["class1" "class2"] :href "http://somelink"} "anchor"])

  (compile-compound [:a ^:attrs (merge {:class "v"})])
   
  (parse-keyword :div.class1.class2)

)
