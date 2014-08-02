(ns dommy.attrs
  (:use-macros
   [dommy.macros :only [node]])
  (:require
   [clojure.string :as str]
   [dommy.utils :refer [as-str]]))

(defn- ^boolean class-match?
  "does class-name string have class starting at index idx.
   only will be used when Element::classList doesn't exist"
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
    only will be used when Element::classList doesn't exist"
  [class-name class]
  (loop [start-from 0]
    (let [i (.indexOf class-name class start-from)]
      (when (>= i 0)
        (if (class-match? class-name class i)
          i
          (recur (+ i (.-length class))))))))

(defn ^boolean has-class?
  "Does an HTML element have a class. Uses Element::classList if
   available and otherwise does fast parse of className string"
  [elem class]
  (let [elem (node elem)
        class (as-str class)]
    (if-let [class-list (.-classList elem)]
      (.contains class-list class)
      (when-let [class-name (.-className elem)]
        (when-let [i (class-index class-name class)]
          (>= i 0))))))

(defn add-class!
  "add class to element"
  ([elem classes]
     (let [elem (node elem)
           classes (-> classes as-str str/trim)]
       (when (seq classes)
         (if-let [class-list (.-classList elem)]
           (doseq [class (.split classes #"\s+")]
             (.add class-list class))
           (doseq [class (.split classes #"\s+")]
             (let [class-name (.-className elem)]
               (when-not (class-index class-name class)
                 (set! (.-className elem)
                       (if (identical? class-name "")
                         class
                         (str class-name " " class))))))))
       elem))
  ([elem classes & more-classes]
     (let [elem (node elem)]
       (doseq [c (conj more-classes classes)]
         (add-class! elem c))
       elem)))

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
  "remove class from and returns `elem`"
  ([elem class]
     (let [elem (node elem)
           class (as-str class)]
       (if-let [class-list (.-classList elem)]
         (.remove class-list class)
         (let [class-name (.-className elem)
               new-class-name (remove-class-str class-name class)]
           (when-not (identical? class-name new-class-name)
             (set! (.-className elem) new-class-name))))
       elem))
  ([elem class & classes]
     (let [elem (node elem)]
       (doseq [c (conj classes class)]
         (remove-class! elem c)))))

(defn toggle-class!
  "(toggle-class! elem class) will add-class! if elem does not have class
   and remove-class! otherwise.
   (toggle-class! elem class add?) will add-class! if add? is truthy,
   otherwise it will remove-class!"
  ([elem class]
     (let [elem (node elem)
           class (as-str class)]
       (if-let [class-list (.-classList elem)]
         (.toggle class-list class)
         (toggle-class! elem class (not (has-class? elem class))))
       elem))
  ([elem class ^boolean add?]
     (let [elem (node elem)]
       (if add?
         (add-class! elem class)
         (remove-class! elem class))
       elem)))

(defn- style-str [x]
  (if (string? x)
    x
    (->> x
         (map (fn [[k v]] (str (as-str k) ":" (as-str v) ";")))
         (str/join " "))))

(defn set-style! [elem & kvs]
  (assert (even? (count kvs)))
  (let [elem (node elem)
        style (.-style elem)]
    (doseq [[k v] (partition 2 kvs)]
      (.setProperty style (as-str k) v))
    elem))

(defn style [elem k]
  (assert k)
  (aget (js/window.getComputedStyle (node elem)) (name k)))

(defn set-px! [elem & kvs]
  (assert (even? (count kvs)))
  (let [elem (node elem)]
    (doseq [[k v] (partition 2 kvs)]
      (set-style! elem k (str v "px")))
    elem))

(defn px [elem k]
  (let [pixels (style (node elem) k)]
    (when (seq pixels)
      (js/parseInt pixels))))

(defn set-attr!
  "Sets dom attributes on and returns `elem`.
   Attributes without values will be set to \"true\":

       (set-attr! elem :disabled)

   With values, the function takes variadic kv pairs:

       (set-attr! elem :id \"some-id\"
                       :name \"some-name\")"
  ([elem k] (set-attr! (node elem) k "true"))
  ([elem k v]
     (when v
       (if (fn? v)
         (doto (node elem)
           (aset (as-str k) v))
         (doto (node elem)
           (.setAttribute
            (as-str k)
            (if (= k :style)
              (style-str v)
              v))))))
  ([elem k v & kvs]
     (assert (even? (count kvs)))
     (let [elem (node elem)]
       (doseq [[k v] (->> kvs (partition 2) (cons [k v]))]
         (set-attr! elem k v))
       elem)))

(defn remove-attr!
  ([elem k]
     (let [elem (node elem)]
       (if (#{:class :classes} k)
         (set! (.-className elem) "")
         (.removeAttribute elem (as-str k)))
       elem))
  ([elem k & ks]
     (let [elem (node elem)]
       (doseq [k (cons k ks)]
         (remove-attr! elem k))
       elem)))

(defn attr [elem k]
  (when k
    (.getAttribute (node elem) (as-str k))))

(defn toggle-attr!
  ([elem k]
     (toggle-attr! elem k (boolean (attr elem k))))
  ([elem k ^boolean add?]
     (let [elem (node elem)]
       (if add?
         (set-attr! elem k)
         (remove-attr! elem k)))))

(defn ^boolean hidden? [elem]
  (identical? "none" (-> (node elem) .-style .-display)))

(defn toggle!
  "Display or hide the given `elem`. Takes an optional boolean `show?`
   indicating whether to show or hide `elem`."
  ([elem ^boolean show?]
     (doto (node elem)
       (-> .-style .-display (set! (if show? "" "none")))))
  ([elem]
     (let [elem (node elem)]
       (toggle! elem (hidden? elem))
       elem)))

(defn hide! [elem]
  (doto (node elem) (toggle! false)))

(defn show! [elem]
  (doto (node elem) (toggle! true)))

(defn bounding-client-rect
  "Returns a map of the bounding client rect of `elem`
   as a map with [:top :left :right :bottom :width :height]"
  [elem]
  (let [r (.getBoundingClientRect (node elem))]
    {:top (.-top r)
     :bottom (.-bottom r)
     :left (.-left r)
     :right (.-right r)
     :width (.-width r)
     :height (.-height r)}))

(defn scroll-into-view
  [elem align-with-top?]
  (let [elem (node elem)
        top (:top (bounding-client-rect elem))]
    (when (< js/window.innerHeight
             (+ top (.-offsetHeight elem)))
      (.scrollIntoView elem align-with-top?))))
