(ns dommy.core
  "Core DOM manipulation functions

   Many of these functions take something which is node-like. Node-like
   refers to the result of calling `dommy.template/->node-like` on the object. For
   any DOM node, ->node-like returns the same reference equals object. When it gets
   passed nested data structure it converts to a fresh DOM node. It falls back to the PElement
   protocol (see dommy.template) so is extensible."
  (:use-macros
   [dommy.macros :only [sel sel1]])
  (:require
   [clojure.string :as str]
   [dommy.utils :as utils]
   [dommy.attrs :as attrs]
   [dommy.template :as template]))

;; Shadowing names from dommy.attrs
;; for backwards compatibility
(def has-class? attrs/has-class?)
(def add-class! attrs/add-class!)
(def remove-class! attrs/remove-class!)
(def toggle-class! attrs/toggle-class!)
(def set-attr! attrs/set-attr!)
(def set-style! attrs/set-style!)
(def set-px! attrs/set-px!)
(def px attrs/px)
(def style-str attrs/style-str)
(def style attrs/style)
(def remove-attr! attrs/remove-attr!)
(def toggle-attr! attrs/toggle-attr!)
(def attr attrs/attr)
(def hidden? attrs/hidden?)
(def toggle! attrs/toggle!)
(def hide! attrs/hide!)
(def show! attrs/show!)
(def bounding-client-rect attrs/bounding-client-rect)
(def scroll-into-view attrs/scroll-into-view)
(def dissoc-in utils/dissoc-in)
(def ->Array utils/->Array)

(defn set-html!
  [elem html]
  (let [elem (template/->node-like elem)]
    (set! (.-innerHTML elem) html)
    elem))

(defn html [elem]
  (-> elem template/->node-like .-innerHTML))

(defn set-text!
  [elem text]
  (let [elem (template/->node-like elem)
        prop (if (.-textContent elem) "textContent" "innerText")]
    (aset elem prop text)
    elem))

(defn text [elem]
  (or (.-textContent elem) (.-innerText elem)))

(defn value [elem]
  (-> elem template/->node-like .-value))

(defn set-value!
  [elem value]
  (let [elem (template/->node-like elem)]
    (set! (.-value elem) value)
    elem))

(defn append!
  "append `child` to `parent`. 'parent' and 'child' should be node-like
   (work with dommy.template/->node-like). The node-like projection
   of parent is returned after appending child."
  ([parent child]
     (doto (template/->node-like parent)
       (.appendChild (template/->node-like child))))

  ([parent child & more-children]
     (let [parent (template/->node-like parent)]
       (doseq [c (cons child more-children)]
         (append! parent c))
       parent)))

(defn prepend!
  "prepend `child` to `parent`, both node-like
   return ->node-like projection of `parent`"
  ([parent child]
     (let [parent (template/->node-like parent)]
       (.insertBefore parent
                      (template/->node-like child)
                      (.-firstChild parent))))

  ([parent child & more-children]
     (let [parent (template/->node-like parent)]
       (doseq [c (cons child more-children)]
         (prepend! parent c))
       parent)))

(defn insert-before!
  "insert `node` before `other`, both node-like,
   `other` must have a parent. return `node`"
  [elem other]
  (let [actual-node (template/->node-like elem)
        other (template/->node-like other)]
    (assert (.-parentNode other))
    (.insertBefore (.-parentNode other) actual-node other)
    actual-node))

(defn insert-after!
  "insert `node` after `other`, both node-like,
   `other` must have a parent. return `node`"
  [elem other]
  (let [actual-node (template/->node-like elem)
        other (template/->node-like other)
        parent (.-parentNode other)]
    (if-let [next (.-nextSibling other)]
      (.insertBefore parent actual-node next)
      (.appendChild parent actual-node))
    actual-node))

(defn replace!
  "replace `elem` with `new`, both node-like, return node-like projection of new.
   node-like projection of elem must have parent."
  [elem new]
  (let [new (template/->node-like new)
        elem (template/->node-like elem)]
    (assert (.-parentNode elem))
    (.replaceChild (.-parentNode elem) new elem)
    new))

(defn replace-contents!
  [parent node-like]
  (doto (template/->node-like parent)
    (-> .-innerHTML (set! ""))
    (append! node-like)))

(defn remove!
  "remove node-like `elem` from parent, return node-like projection of elem"
  [elem]
  (let [elem (template/->node-like elem)]
    (doto (.-parentNode elem)
      (.removeChild elem))))

(defn clear!
  "clears all children from `elem`"
  [elem]
  (set! (.-innerHTML (template/->node-like elem)) ""))

(defn selector [data]
  (cond
   (coll? data) (clojure.string/join " " (map selector data))
   (or (string? data) (keyword? data)) (name data)))

(defn selector-map [template key-selectors-map]
  (let [container (dommy.template/->node-like template)]
    (assert (not (contains? key-selectors-map :container)))
    (->>  key-selectors-map
          (map (fn [[k v]]
                 [k
                  (if (:live (meta v))
                    (reify
                      IDeref
                      (-deref [this] (sel container v)))
                    (sel1 container v))]))
          (into {})
          (merge {:container container}))))

(defn ancestor-nodes
  "a lazy seq of the ancestors of `node`"
  [elem]
  (->> (template/->node-like elem)
       (iterate #(.-parentNode %))
       (take-while identity)))

(defn matches-pred
  "returns a predicate on nodes that match `selector` at the
   time of this `matches-pred` call (may return outdated results
   if you fuck with the DOM)"
  ([base selector]
     (let [matches (sel (template/->node-like base) selector)]
       (fn [elem]
         (-> matches (.indexOf elem) (>= 0)))))
  ([selector]
     (matches-pred js/document selector)))

(defn closest
  "closest ancestor of `node` (up to `base`, if provided)
   that matches `selector`"
  ([base elem selector]
     (let [base (template/->node-like base)
           elem (template/->node-like elem)]
       (->> (ancestor-nodes elem)
            (take-while #(not (identical? % base)))
            (filter (matches-pred base selector))
            first)))
  ([elem selector]
     (first (filter (matches-pred selector) (ancestor-nodes (template/->node-like elem))))))

(defn ^boolean descendant?
  "is `descendant` a descendant of `ancestor`?"
  [descendant ancestor]
  ;; http://www.quirksmode.org/blog/archives/2006/01/contains_for_mo.html
  (let [descendant (template/->node-like descendant)
        ancestor (template/->node-like ancestor)]
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
    (let [selected-target (closest (template/->node-like elem) (.-target event) selector)]
      (when (and selected-target (not (attr selected-target :disabled)))
        (set! (.-selectedTarget event) selected-target)
        (f event)))))

(defn- event-listeners
  "Returns a nested map of event listeners on `nodes`"
  [elem]
  (or (.-dommyEventListeners (template/->node-like elem)) {}))

(defn- update-event-listeners!
  [elem f & args]
  (let [elem (template/->node-like elem)]
    (set! (.-dommyEventListeners elem)
          (apply f (event-listeners elem) args))))

(defn- elem-and-selector
  [elem-sel]
  (if (sequential? elem-sel)
    ((juxt #(template/->node-like (first %)) rest) elem-sel)
    [(template/->node-like elem-sel) nil]))

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
      (update-event-listeners! elem utils/dissoc-in keys)
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
         (f e)))))
  elem-sel)

(defn fire!
  "NOTE: ONLY TO BE USED FOR TESTS. May not work at mocking many
   event types or their bubbling behaviours

   Creates an event of type `event-type`, optionally having
   `update-event!` mutate and return an updated event object,
   and fires it on `node`.
   Only works when `node` is in the DOM"
  [node event-type & [update-event!]]
  (assert (descendant? node js/document.documentElement))
  (let [update-event! (or update-event! identity)]
    (if (.-createEvent js/document)
      (let [event (.createEvent js/document "Event")]
        (.initEvent event (name event-type) true true)
        (.dispatchEvent node (update-event! event)))
      (.fireEvent node (str "on" (name event-type))
                  (update-event! (.createEventObject js/document))))))
