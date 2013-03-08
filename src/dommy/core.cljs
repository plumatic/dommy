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
(def set-attr! attrs/set-attr!)
(def remove-attr! attrs/remove-attr!)
(def hidden? attrs/hidden?)
(def toggle! attrs/toggle!)

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

(defn insert-before!
  "insert `node` before `other`, both DOM nodes,
   `other` must have a parent. return `node`"
  [node other]
  (let [actual-node (template/->node-like node)]
    (.insertBefore (.-parentNode other) actual-node other)
    actual-node))

(defn insert-after!
  "insert `node` after `other`, both DOM nodes,
   `other` must have a parent. return `node`"
  [node other]
  (let [actual-node (template/->node-like node)
        parent (.-parentNode other)]
    (if-let [next (.-nextSibling other)]
      (.insertBefore parent actual-node next)
      (.appendChild parent actual-node))
    actual-node))

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

(defn descendant?
  "is `descendant` a descendant of `ancestor`?"
  [descendant ancestor]
  ;; http://www.quirksmode.org/blog/archives/2006/01/contains_for_mo.html
  (cond (.-contains ancestor)
          (.contains ancestor descendant)
        (.-compareDocumentPosition ancestor)
          (-> (.compareDocumentPosition ancestor descendant)
              (bit-test 4))))

(def special-listener-makers
  (->> {:mouseenter :mouseover
        :mouseleave :mouseout}
       (map (fn [[special-mouse-event real-mouse-event]]
              [special-mouse-event
               {real-mouse-event
                (fn [f]
                  (fn [event]
                    (let [related-target (.-relatedTarget event)
                          listener-target (or (.-selectedTarget event)
                                              (.-currentTarget event))]
                      (when-not (and related-target
                                     (descendant? related-target listener-target))
                        (f event)))))}]))
       (into {})))

(defn live-listener
    "fires f if event.target is found within the specified selector"
    [node selector f]
    (fn [e]
      (when (-> (sel node selector)
                (.indexOf (.-target e))
                (>= 0))
        (f e))))

(defn- event-listeners
  "Returns a nested map of event listeners on `nodes`"
  [node]
  (or (.-dommyEventListeners node) {}))

(defn- update-event-listeners!
  [node f & args]
  (set! (.-dommyEventListeners node)
        (apply f (event-listeners node) args)))

(defn- node-and-selector
  [node-sel]
  (if (sequential? node-sel)
    ((juxt first rest) node-sel)
    [node-sel nil]))

(defn listen!
  "Adds `f` as a listener for events of type `event-type` on
   `node-sel`, which must either be a DOM node, or a sequence
   whose first item is a DOM node.

   In other words, the call to `listen!` can take two forms:

   If `node-sel` is a DOM node, i.e., you're doing something like:

       (listen! node :click click-handler)

   then `click-handler` will be set as a listener for `click` events
   on the `node`.

   If `node-sel` is a sequence:

       (listen! [node :.selector.for :.some.descendants] :click click-handler)

   then `click-handler` will be set as a listener for `click` events
   on descendants of `node` that match the selector

   Also accepts any number of event-type and handler pairs for setting
   multiple listeners at once:

       (listen! some-node :click click-handler :hover hover-handler)"
  [node-sel & type-fs]
  (assert (even? (count type-fs)))
  (let [[node selector] (node-and-selector node-sel)]
    (doseq [[orig-type f] (partition 2 type-fs)
            [actual-type factory] (get special-listener-makers orig-type {orig-type identity})
            :let [canonical-f (-> f
                                  factory
                                  ((if selector
                                     (partial live-listener node selector)
                                     identity)))]]
      (update-event-listeners! node assoc-in [selector actual-type f] canonical-f)
      (if (.-addEventListener node)
        (.addEventListener node (name actual-type) canonical-f)
        ;; For IE < 9
        (.attachEvent node (name actual-type) canonical-f)))))

(defn unlisten!
  "Removes event listener for the node defined in `node-sel`,
   which is the same format as listen!.

  The following forms are allowed, and will remove all handlers
  that match the parameters passed in:
  
      (unlisten! [node :.selector] :click event-listener)

      (unlisten! [node :.selector]
        :click event-listener
        :mouseover other-event-listener)"
  [node-sel & type-fs]
  (assert (even? (count type-fs)))
  (let [[node selector] (node-and-selector node-sel)]
    (doseq [[orig-type f] (partition 2 type-fs)
            [actual-type _] (get special-listener-makers orig-type {orig-type identity})
            :let [keys [selector actual-type f]
                  canonical-f (get-in (event-listeners node) keys)]]
      (update-event-listeners! node dissoc-in keys)
      (if (.-removeEventListener node)
        (.removeEventListener node (name actual-type) canonical-f)
        ;; For IE < 9
        (.detachEvent node (name actual-type) canonical-f)))))

(defn listen-once!
  [node-sel & type-fs]
  (assert (even? (count type-fs)))
  (let [[node selector] (node-and-selector node-sel)]
    (doseq [[type f] (partition 2 type-fs)]
      (listen!
       node-sel type
       (fn this-fn [e]
         (unlisten! node-sel type this-fn)
         (f e))))))

