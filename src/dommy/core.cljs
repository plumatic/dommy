(ns dommy.core
  (:use-macros
   [dommy.macros.core :only [sel]])
  (:require
   [clojure.string :as str]
   [dommy.attrs :as attrs]))

(def has-class? attrs/has-class?)
(def add-class! attrs/add-class!)
(def remove-class! attrs/remove-class!)
(def toggle-class! attrs/toggle-class!)
(def add-attr! attrs/add-attr!)
(def hidden? attrs/hidden?)
(def toggle! attrs/toggle!)

(defn ->Array [array-like]
  (.call js/Array.prototype.slice array-like))

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