(ns dommy.utils)

(defn dissoc-in
  "Dissociate this keyseq from m, removing any empty maps created as a result
   (including at the top-level)."
  [m [k & ks]]
  (when m
    (if-let [res (and ks (dissoc-in (m k) ks))]
      (assoc m k res)
      (let [res (dissoc m k)]
        (when-not (empty? res)
          res)))))

(defn ->Array [array-like]
  (.call js/Array.prototype.slice array-like))

(defn as-str
  "Coerces strings and keywords to strings, while preserving namespace of
   namespaced keywords"
  [s]
  (if (keyword? s)
    (str (some-> (namespace s) (str "/")) (name s))
    s))
