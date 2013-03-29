(ns dommy.core
  (:use-macros
   [dommy.macros :only [sel node]])
  (:require
   [clojure.string :as str]
   [dommy.attrs :as attrs]
   [dommy.template :as template]))

(def has-class? attrs/has-class?)
(def add-class! attrs/add-class!)
(def remove-class! attrs/remove-class!)
(def toggle-class! attrs/toggle-class!)
(def set-style! attrs/set-style!)
(def style attrs/style)
(def set-attr! attrs/set-attr!)
(def set-style! attrs/set-style!)
(def set-px! attrs/set-px!)
(def px attrs/px)
(def style-str attrs/style-str)
(def style attrs/style)
(def remove-attr! attrs/remove-attr!)
(def attr attrs/attr)
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
  (doto (node parent)
    (.appendChild (template/->node-like child))))

(defn prepend!
  "prepend `child` to `parent`, both DOM nodes. return `parent`"
  [parent child]
  (doto (node parent)
    (.insertBefore (template/->node-like child)
                   (.-firstChild parent))))

(defn insert-before!
  "insert `node` before `other`, both DOM nodes,
   `other` must have a parent. return `node`"
  [elem other]
  (let [actual-node (template/->node-like elem)
        other (node other)]
    (.insertBefore (.-parentNode other) actual-node other)
    actual-node))

(defn insert-after!
  "insert `node` after `other`, both DOM nodes,
   `other` must have a parent. return `node`"
  [elem other]
  (let [actual-node (template/->node-like elem)
        other (node other)
        parent (.-parentNode other)]
    (if-let [next (.-nextSibling other)]
      (.insertBefore parent actual-node next)
      (.appendChild parent actual-node))
    actual-node))

(defn replace!
  "replace `node` with new (made from `data`), return new"
  [elem data]
  (let [new (template/->node-like data)
        elem (node elem)]
    (.replaceChild (.-parentNode elem) new elem)
    new))

(defn replace-contents!
  [parent node-like]
  (doto (node parent)
    (-> .-innerHTML (set! ""))
    (append! node-like)))

(defn remove!
  "remove `node` from parent, return parent"
  [elem]
  (let [elem (node elem)]
    (doto (.-parentNode elem)
      (.removeChild elem))))

(defn selector [data]
  (cond
   (coll? data) (clojure.string/join " " (map selector data))
   (or (string? data) (keyword? data)) (name data)))

(defn ancestor-nodes
  "a lazy seq of the ancestors of `node`"
  [elem]
  (->> (node elem)
       (iterate #(.-parentNode %))
       (take-while identity)))

(defn matches-pred
  "returns a predicate on nodes that match `selector` at the
   time of this `matches-pred` call (may return outdated results
   if you fuck with the DOM)"
  ([base selector]
   (let [matches (sel (node base) selector)]
     (fn [elem]
       (-> matches (.indexOf elem) (>= 0)))))
  ([selector]
   (matches-pred js/document selector)))

(defn closest
  "closest ancestor of `node` (up to `base`, if provided)
   that matches `selector`"
  ([base elem selector]
   (let [base (node base)
         elem (node elem)]
     (->> (ancestor-nodes elem)
          (take-while #(not (identical? % base)))
          (filter (matches-pred base selector))
          first)))
  ([elem selector]
     (first (filter (matches-pred selector) (ancestor-nodes (node elem))))))

(defn descendant?
  "is `descendant` a descendant of `ancestor`?"
  [descendant ancestor]
  ;; http://www.quirksmode.org/blog/archives/2006/01/contains_for_mo.html
  (let [descendant (node descendant)
        ancestor (node ancestor)]
    (cond (.-contains ancestor)
          (.contains ancestor descendant)
          (.-compareDocumentPosition ancestor)
          (-> (.compareDocumentPosition ancestor descendant)
              (bit-test 4)))))

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
  "fires f if event.target is found with `selector`"
  [elem selector f]
  (fn [event]
    (when-let [selected-target (closest (node elem) (.-target event) selector)]
      (set! (.-selectedTarget event) selected-target)
      (f event))))

(defn- event-listeners
  "Returns a nested map of event listeners on `nodes`"
  [elem]
  (or (.-dommyEventListeners (node elem)) {}))

(defn- update-event-listeners!
  [elem f & args]
  (let [elem (node elem)]
    (set! (.-dommyEventListeners elem)
          (apply f (event-listeners elem) args))))

(defn- elem-and-selector
  [elem-sel]
  (if (sequential? elem-sel)
    ((juxt #(node (first %)) rest) elem-sel)
    [(node elem-sel) nil]))

(defn listen!
  "Adds `f` as a listener for events of type `event-type` on
   `elem-sel`, which must either be a DOM node, or a sequence
   whose first item is a DOM node.

   In other words, the call to `listen!` can take two forms:

   If `elem-sel` is a DOM node, i.e., you're doing something like:

       (listen! elem :click click-handler)

   then `click-handler` will be set as a listener for `click` events
   on the `elem`.

   If `elem-sel` is a sequence:

       (listen! [elem :.selector.for :.some.descendants] :click click-handler)

   then `click-handler` will be set as a listener for `click` events
   on descendants of `elem` that match the selector

   Also accepts any number of event-type and handler pairs for setting
   multiple listeners at once:

       (listen! some-elem :click click-handler :hover hover-handler)"
  [elem-sel & type-fs]
  (assert (even? (count type-fs)))
  (let [[elem selector] (elem-and-selector elem-sel)]
    (doseq [[orig-type f] (partition 2 type-fs)
            [actual-type factory] (get special-listener-makers orig-type {orig-type identity})
            :let [canonical-f (-> f
                                  factory
                                  ((if selector
                                     (partial live-listener elem selector)
                                     identity)))]]
      (update-event-listeners! elem assoc-in [selector actual-type f] canonical-f)
      (if (.-addEventListener elem)
        (.addEventListener elem (name actual-type) canonical-f)
        ;; For IE < 9
        (.attachEvent elem (name actual-type) canonical-f))))
  elem-sel)

(defn unlisten!
  "Removes event listener for the element defined in `elem-sel`,
   which is the same format as listen!.

  The following forms are allowed, and will remove all handlers
  that match the parameters passed in:

      (unlisten! [elem :.selector] :click event-listener)

      (unlisten! [elem :.selector]
        :click event-listener
        :mouseover other-event-listener)"
  [elem-sel & type-fs]
  (assert (even? (count type-fs)))
  (let [[elem selector] (elem-and-selector elem-sel)]
    (doseq [[orig-type f] (partition 2 type-fs)
            [actual-type _] (get special-listener-makers orig-type {orig-type identity})
            :let [keys [selector actual-type f]
                  canonical-f (get-in (event-listeners elem) keys)]]
      (update-event-listeners! elem dissoc-in keys)
      (if (.-removeEventListener elem)
        (.removeEventListener elem (name actual-type) canonical-f)
        ;; For IE < 9
        (.detachEvent elem (name actual-type) canonical-f))))
  elem-sel)

(defn listen-once!
  [elem-sel & type-fs]
  (assert (even? (count type-fs)))
  (let [[elem selector] (elem-and-selector elem-sel)]
    (doseq [[type f] (partition 2 type-fs)]
      (listen!
       elem-sel type
       (fn this-fn [e]
         (unlisten! elem-sel type this-fn)
         (f e))))))
