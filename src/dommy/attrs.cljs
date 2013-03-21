(ns dommy.attrs
  (:require
   [clojure.string :as str]))

(defn- class-match?
  "does class-name string have class starting at index idx.
   only will be used when node.classList doesn't exist"
  [class-name class idx]
  (and
   ;; start
   (or (zero? idx) (identical? \space (.charAt class-name (dec idx))))
   ;; stop
   (let [total-len (.-length class-name)
         stop (+ idx (.-length class))]
     (when (<= stop total-len)
       (or (identical? stop total-len)
           (identical? \space (.charAt class-name stop)))))))

(defn- class-index
  "Finds the index of class in a space-delimited class-name
    only will be used when node.classList doesn't exist"
  [class-name class]
  (loop [start-from 0]
    (let [i (.indexOf class-name class start-from)]
      (when (>= i 0)
        (if (class-match? class-name class i)
          i
          (recur (+ i (.-length class))))))))

(defn has-class?
  "Does DOM node have class. Uses node.classList if available
   and otherwise does fast parse of className string"
  [node class]
  (if-let [class-list (.-classList node)]
    (.contains class-list class)
    (when-let [class-name (.-className node)]
      (when-let [i (class-index class-name class)]
        (>= i 0)))))

(defn add-class!
  "add class to node"
  [node class]
  (if-let [class-list (.-classList node)]
    (.add class-list class)
    (let [class-name (.-className node)]
      (when-not (class-index class-name class)
        (set! (.-className node)
              (if (identical? class-name "")
                class
                (str class-name " " class))))))
  node)

(defn- remove-class-str [init-class-name class]
  (loop [class-name init-class-name]
    (let [class-len (.-length class-name)]
      (if-let [i (class-index class-name class)]
        (recur (let [end (+ i (.-length class))]
                 (str (if (< end class-len)
                        (str (.substring class-name 0 i) (.substr class-name (inc end)))
                        (.substring class-name 0 (dec i))))))
        class-name))))

(defn remove-class!
  "remove class from and returns `node`"
  [node class]
  (if-let [class-list (.-classList node)]
    (.remove  class-list class)
    (let [class-name (.-className node)
          new-class-name (remove-class-str class-name (name class))]
    (when-not (identical? class-name new-class-name)
      (set! (.-className node) new-class-name))))
  node)

(defn toggle-class!
  "(toggle-class! node class) will add-class! if node does not have class
   and remove-class! otherwise.
   (toggle-class! node class add?) will add-class! if add? is truthy,
   otherwise it will remove-class!"
  ([node class]
     (if-let [class-list (.-classList node)]
       (.toggle class-list class)
       (toggle-class! node class (not (has-class? node class)))))
  ([node class add?]
     (if add?
       (add-class! node class)
       (remove-class! node class))))

(defn- style-str [m]
  (->> m
       (map (fn [[k v]] (str (name k) ":" (name v) ";")))
       (str/join " ")))

(defn set-style! [node & kvs]
  (assert (even? (count kvs)))
  (let [style (.-style node)]
    (doseq [[k v] (partition 2 kvs)]
      (aset style (name k) v))
    node))

(defn style [node k]
  (assert k)
  (aget (js/window.getComputedStyle node) (name k)))

(defn set-px! [node & kvs]
  (assert (even? (count kvs)))
  (doseq [[k v] (partition 2 kvs)]
    (set-style! node k (str v "px"))))

(defn px [node k]
  (js/parseInt (style node k)))

(defn set-attr!
  "Sets dom attributes on and returns `node`.
   Attributes without values will be set to \"true\":

       (set-attr! node :disabled)

   With values, the function takes variadic kv pairs:

       (set-attr! node :id \"some-id\"
                       :name \"some-name\")"
  ([node k] (set-attr! node k "true"))
  ([node k v]
     (when v
       (.setAttribute
        node (name k)
        (if (identical? k :style)
          (style-str v)
          v)))
     node)
  ([node k v & kvs]
     (assert (even? (count kvs)))
     (doseq [[k v] (->> kvs (partition 2) (cons [k v]))]
       (set-attr! node k v))
     node))

(defn remove-attr!
  ([node k]
     (if (#{:class :classes} k)
       (set! (.-className node) "")
       (.removeAttribute node (name k)))
     node)
  ([node k & ks]
     (doseq [k (cons k ks)]
       (remove-attr! node k))
     node))

(defn attr [node k]
  (when k
    (.getAttribute node (name k))))

(defn hidden? [node]
  (identical? "none" (-> node .-style .-display)))

(defn toggle!
  "Display or hide the given `node`. Takes an optional boolean `show?`
   indicating whether to show or hide `node`."
  ([node show?]
     (set! (-> node .-style .-display) (if show? "" "none"))
     node)
  ([node]
     (toggle! node (hidden? node))))

