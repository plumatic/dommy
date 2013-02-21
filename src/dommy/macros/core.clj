(ns dommy.macros.core)

(defn constant? [data]  
  (cond
   (coll? data) (every? constant? data)
   (some #(% data) [number? keyword? string?]) true))

(defn selector [data]
  (cond
   (coll? data) (clojure.string/join " " (map selector data))
   (or (string? data) (keyword? data)) (name data)))

(defn selector-form [data]
  (if (constant? data)
     (selector data)
     `(dommy.core/selector ~data)))

(defmacro sel1
  ([base data]
     `(.querySelector ~base ~(selector-form data)))
  ([data]
     `(sel1 js/document ~data)))

(defmacro sel 
  ([base data]
     `(dommy.core/->Array
       (.querySelectorAll ~base ~(selector-form data))))
  ([data]
     `(sel js/document ~data)))
