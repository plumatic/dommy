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

(defn- selector [data]
  (cond
   (coll? data) (clojure.string/join " " (map selector data))
   (or (string? data) (keyword? data)) (name data)))

(defn- class-match?
  "does class-name string have class starting at index idx"
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
  "Finds the index of class in a space-delimited class-name"
  [class-name class]
  (loop [start-from 0]
    (let [i (.indexOf class-name class start-from)]
      (when (>= i 0)
        (if (class-match? class-name class i)
          i
          (recur (+ i (.-length class))))))))

(defn has-class?
  "does DOM node have class"
  [node class]
  (when-let [i (class-index (.-className node) class)]
    (>= i 0)))

(defn add-class!
  "add class to node c"
  [node c]
  (let [class-name (.-className node)]
    (when-not (class-index class-name c)
      (set! (.-className node)
            (if (identical? class-name "")
              c
              (str class-name " " c))))))

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
  (let [class-name (.-className node)
        new-class-name (remove-class-str class-name class)]
    (when-not (identical? class-name new-class-name)
      (set! (.-className node) new-class-name))))

(defn toggle-class!
  "(toggle-class! node class) will add-class! if node does not have class and remove-class! otherwise.
   
   (toggle-class! node class pred) will add-class! if pred is a function and (pred node class) returns truthy, or
   pred is truthy. Otherwise, it will remove-class!"
  ([node class]
     (if (has-class? node class)
       (remove-class! node class)
       (add-class! node class)))
  ([node class pred]
     (if (if (fn? pred) (pred class node) pred)
       (add-class! node class)
       (remove-class! node class))))

(defn live-listener [node selector f]
  (fn [event]
    ;; does event.target match selector 
    (when (-> (sel node live-selector) (#(apply array %)) (.indexOf (.-target event)) (>= 0))
      (f event))))

;; (defprotocol PEventListener
;;   (listener-id [this] "id for the listener"))

;; (defn named-listener [key f]
;;   (reify
;;     PEventListener
;;     (listener-id [this] key)
;;     IFn
;;     (invoke [& args] (apply f args))))

(defn add-listen!
  ([node event-type live-selector f]
     (listen! node event-type (live-listener node live-selector f)))
  ([node event-type f]
     (.addEventListener node (name event-type) f)))
