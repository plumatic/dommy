(ns dommy.utils
  (:require
   [clojure.string :as str]))

(defn as-str
  "Coerces strings and keywords to strings, while preserving namespace of
   namespaced keywords"
  [s]
  (if (keyword? s)
    (str (some-> (namespace s) (str "/")) (name s))
    s))

(defn constant? [data]
  (some #(% data) [number? keyword? string?]))

(defn all-constant? [data]
  (cond
   (coll? data) (every? all-constant? data)
   (constant? data) true))

(defn single-selector? [data]
  (re-matches #"^\S+$" (as-str data)))

(defn id-selector? [s]
  (re-matches #"^#[\w-]+$" (as-str s)))

(defn class-selector? [s]
  (re-matches #"^\.[a-z_-][a-z0-9_-]*$" (as-str s)))

(defn tag-selector? [s]
  (re-matches #"^[a-z_-][a-z0-9_-]*$" (as-str s)))

(defn selector [data]
  (cond
   (coll? data) (str/join " " (map selector data))
   (constant? data) (as-str data)))

(defn selector-form [data]
  (if (all-constant? data)
    (selector data)
    `(dommy.core/selector ~data)))

(defn query-selector [base data]
  `(.querySelector ~base ~(selector-form data)))

(defn query-selector-all [base data]
  `(dommy.utils/->Array
    (.querySelectorAll ~base ~(selector-form data))))
