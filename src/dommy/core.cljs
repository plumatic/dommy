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

(let [live-listeners (atom {})]

  (defn live-listener
    "fires f if event.target is found within the specified selector"
    [node selector f]
    (fn [e]
      (when (-> (sel node selector)
                (.indexOf (.-target e))
                (>= 0))
        (f e))))

  (defn unlisten!
    ([node event-type live-selector f]
       (let [listener-key [node event-type live-selector f]
             live-fn (@live-listeners listener-key)]
         (swap! live-listeners dissoc listener-key)
         (unlisten! node event-type live-fn)))
    ([node event-type f]
       (.removeEventListener node (name event-type) f)))

  (defn listen!
    ([node event-type live-selector f]
       (let [live-fn (live-listener node live-selector f)]
         (swap! live-listeners assoc
                [node event-type live-selector f]
                live-fn)
         (listen! node event-type live-fn)))
    ([node event-type f]
       (if (.-addEventListener node)
         (.addEventListener node (name event-type) f)
         ;; fucking ie <= 8
         (.attachEvent node (name event-type) f))))

  (defn listen-once!
    ([node event-type live-selector f]
       (listen!
        node event-type live-selector
        (fn [e]
          (unlisten! node event-type live-selector f)
          (f e))))
    ([node event-type f]
       (listen!
        node event-type
        (fn [e]
          (unlisten! node event-type f)
          (f e))))))
