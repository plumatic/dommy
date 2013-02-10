(ns dommy.core
  (:use-macros [dommy.core-compile :only [sel]])
  (:require
   [clojure.string :as str]))

(defn append!
  "append child to parent, both DOM nodes. return parent"
  [parent child]
  (.appendChild parent child)
  parent)

(defn prepend! 
  "prepend child to parent, both DOM nodes. return parent"
  [parent child]
  (.insertBefore parent child (.-firstChild parent))
  parent)

(defn remove!
  "remove node from parent, return parent"
  [node]
  (let [parent (.-parentNode node)]
    (.removeChild parent node)
    parent))

(defn replace!
  "replace node with new, return new"
  [node new]
  (.replaceChild (.-parentNode node) new node)
  new)

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
    (when-let [i (class-index (.-className node) class)]
      (>= i 0))))

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
                (str class-name " " class)))))))

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
  "remove class from node"
  [node class]
  (if-let [class-list (.-classList node)]
    (.remove  class-list class)
    (let [class-name (.-className node)
          new-class-name (remove-class-str class-name (name class))]
    (when-not (identical? class-name new-class-name)
      (set! (.-className node) new-class-name)))))

(defn toggle-class!
  "(toggle-class! node class) will add-class! if node does not have class and remove-class! otherwise.
   (toggle-class! node class add?) will add-class! if add? is truthy, otherwise it will remove-class!"
  ([node class]
     (if-let [class-list (.-classList node)]
       (.toggle class-list class)
       (toggle-class! node class (not (has-class? node class)))))
  ([node class add?]
     (if add?
       (add-class! node class)
       (remove-class! node class))))

(defn selector [data]
  (cond
   (coll? data) (clojure.string/join " " (map selector data))
   (or (string? data) (keyword? data)) (name data)))

(defn live-listener
  "fires f if event.target is found within the specified selector"
  [node selector f]
  (fn [event]
    (when (-> (sel node selector)
              (.indexOf (.-target event))
              (>= 0))
      (f event))))

(defn listen!
  ([node event-type live-selector f]
     (listen! node event-type (live-listener node live-selector f)))
  ([node event-type f]
     (if (.-addEventListener node)
       (.addEventListener node (name event-type) f)
       ;; fucking ie <= 8
       (.attachEvent node (name event-type) f))))