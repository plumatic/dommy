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

(defn has-class?
  "does DOM node have class"
  [node class]
  (.contains (.-classList node) class))

(defn add-class!
  "add class to node"
  [node class]
  (.add (.-classList node) class))

(defn remove-class!
  "remove class from node"
  [node class]
  (.remove (.-classList node) class))

(defn toggle-class!
  "(toggle-class! node class) will add-class! if node does not have class and remove-class! otherwise.
   (toggle-class! node class add?) will add-class! if add? is truthy, otherwise it will remove-class!"
  ([node class]
     (.toggle (.-classList node) class))
  ([node class add?]
     (if add?
       (add-class! node class)
       (remove-class! node class))))

(defn- selector [data]
  (cond
   (coll? data) (clojure.string/join " " (map selector data))
   (or (string? data) (keyword? data)) (name data)))

(defn live-listener
  "fires f if event.target is found within the specified selector"
  [node selector f]
  (fn [event]
    (when (-> (sel node selector) (#(apply array %)) (.indexOf (.-target event)) (>= 0))
      (f event))))

(defn listen!
  ([node event-type live-selector f]
     (listen! node event-type (live-listener node live-selector f)))
  ([node event-type f]
     (.addEventListener node (name event-type) f)))

;; (defprotocol PEventListener
;;   (listener-id [this] "id for the listener"))

;; (defn named-listener [key f]
;;   (reify
;;     PEventListener
;;     (listener-id [this] key)
;;     IFn
;;     (invoke [& args] (apply f args))))


