(ns dommy.core
  (:use-macros
   [dommy.macros :only [sel]])
  (:require
   [clojure.string :as str]
   [dommy.attrs :as attrs]
   [dommy.template :as template]))

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
  "append `child` to `parent`, both DOM nodes. return `parent`"
  [parent child]
  (.appendChild parent (template/->node-like child))
  parent)

(defn prepend!
  "prepend `child` to `parent`, both DOM nodes. return `parent`"
  [parent child]
  (.insertBefore parent
                 (template/->node-like child)
                 (.-firstChild parent))
  parent)

(defn replace!
  "replace node with new, return new"
  [node data]
  (let [new (template/->node-like data)]
    (.replaceChild (.-parentNode node) new node)
    new))

(defn remove!
  "remove node from parent, return parent"
  [node]
  (let [parent (.-parentNode node)]
    (.removeChild parent node)
    parent))

(defn selector [data]
  (cond
   (coll? data) (clojure.string/join " " (map selector data))
   (or (string? data) (keyword? data)) (name data)))

(defn closest
  "closest ancestor of `node` (up to `base`, if provided)
   that matches `selector`"
  ([base node selector]
     (let [matches (sel base selector)]
       (loop [ancestor node]
         (when (not= base ancestor)
           (if (-> matches
                   (.indexOf ancestor)
                   (>= 0))
             ancestor
             (recur (.-parentNode ancestor)))))))
  ([node selector]
     (closest js/document node selector)))

(defn live-listener
  "fires f if event.target is found within the specified selector"
  [node selector f]
  (fn [event]
    (when-let [current-target (closest node (.-target event) selector)]
      (set! (.-currentTarget event) current-target)
      (f event))))

(defn listen!
  ([node event-type live-selector f]
     (listen! node event-type (live-listener node live-selector f)))
  ([node event-type f]
     (if (.-addEventListener node)
       (.addEventListener node (name event-type) f)
       ;; fucking ie <= 8
       (.attachEvent node (name event-type) f))))
