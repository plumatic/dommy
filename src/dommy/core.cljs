(ns dommy.core
  "Core DOM manipulation functions"
  (:refer-clojure :exclude [ancestors class])
  (:require-macros
   [dommy.core :refer [sel]])
  (:require
   [clojure.string :as str]
   [dommy.utils :as utils :refer [as-str]]))

(defn selector
  "Returns a selector in string format.
   Accepts string, keyword, or collection."
  [data]
  (cond
   (coll? data) (str/join " " (map selector data))
   (or (string? data) (keyword? data)) (name data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element accessors

(defn text [elem]
  (or (.-textContent elem) (.-innerText elem)))

(defn html [elem]
  (.-innerHTML elem))

(defn value [elem]
  (.-value elem))

(defn class [elem]
  (.-className elem))

(defn attr [elem k]
  (when k
    (.getAttribute elem (as-str k))))

(defn style
  "The computed style of `elem`, optionally specifying the key of
   a particular style to return"
  ([elem]
     (js->clj (.getComputedStyle js/window elem)))
  ([elem k]
     (aget (.getComputedStyle js/window elem) (as-str k))))

(defn px [elem k]
  "Returns a numeric style attribute as its pixel value"
  (let [pixels (style elem k)]
    (when (seq pixels)
      (js/parseInt pixels))))

(defn ^boolean has-class?
  "Does `elem` contain `c` in its class list"
  [elem c]
  (let [c (utils/as-str c)]
    (if-let [class-list (.-classList elem)]
      (.contains class-list c)
      (when-let [class-name (class elem)]
        (when-let [i (utils/class-index class-name c)]
          (>= i 0))))))

(defn ^boolean hidden?
  "Is `elem` hidden (as associated with hide!/show!/toggle!, using display: none)"
  [elem]
  (identical? (style elem :display) "none"))

(defn bounding-client-rect
  "Returns a map of the bounding client rect of `elem`
   as a map with [:top :left :right :bottom :width :height]"
  [elem]
  (let [r (.getBoundingClientRect elem)]
    {:top (.-top r)
     :bottom (.-bottom r)
     :left (.-left r)
     :right (.-right r)
     :width (.-width r)
     :height (.-height r)}))

(defn parent [elem]
  (.-parentNode elem))

(defn children [elem]
  (.-children elem))

(defn ancestors
  "Lazy seq of the ancestors of `elem`"
  [elem]
  (take-while identity (iterate parent elem)))

(def ^{:deprecated "1.0.0"} ancestor-nodes ancestors)

(defn matches-pred
  "Returns a predicate on nodes that match `selector` at the
   time of this `matches-pred` call (may return outdated results
   if you fuck with the DOM)"
  ([base selector]
     (let [matches (sel base selector)]
       (fn [elem]
         (-> matches (.indexOf elem) (>= 0)))))
  ([selector]
     (matches-pred js/document selector)))

(defn closest
  "Closest ancestor of `elem` (up to `base`, if provided)
   that matches `selector`"
  ([base elem selector]
     (->> (ancestors elem)
          (take-while #(not (identical? % base)))
          (filter (matches-pred base selector))
          first))
  ([elem selector]
     (closest js/document.body elem selector)))

(defn ^boolean descendant?
  "Is `descendant` a descendant of `ancestor`?
   (http://goo.gl/T8pgCX)"
  [descendant ancestor]
  (cond (.-contains ancestor)
        (.contains ancestor descendant)

        (.-compareDocumentPosition ancestor)
        (-> (.compareDocumentPosition ancestor descendant)
            (bit-test 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element modification

(defn set-text!
  "Set the textContent of `elem` to `text`, fall back to innerText"
  [elem text]
  (if-not (undefined? (.-textContent elem))
    (set! (.-textContent elem) text)
    (set! (.-innerText elem) text))
  elem)

(defn set-html!
  "Set the innerHTML of `elem` to `html`"
  [elem html]
  (set! (.-innerHTML elem) html)
  elem)

(defn set-value!
  "Set the value of `elem` to `value`"
  [elem value]
  (set! (.-value elem) value)
  elem)

(defn set-class!
  "Set the css class of `elem` to `elem`"
  [elem c]
  (set! (.-className elem) c))

(defn set-style!
  "Set the style of `elem` using key-value pairs:

      (set-style! elem :display \"block\" :color \"red\")"
  [elem & kvs]
  (assert (even? (count kvs)))
  (let [style (.-style elem)]
    (doseq [[k v] (partition 2 kvs)]
      (.setProperty style (as-str k) v))
    elem))

(defn remove-style!
  "Remove the style of `elem` using keywords:
  
      (remove-style! elem :display :color)"
  [elem & keywords]
  (let [style (.-style elem)]
    (doseq [kw keywords]
      (.removeProperty style (as-str kw)))
    elem))

(defn set-px! [elem & kvs]
  "Set the style of `elem`, converting numeric
   pixel values string pixel values:

       (set-px! elem :top 1337 :left 42)"
  (assert (even? (count kvs)))
  (doseq [[k v] (partition 2 kvs)]
    (set-style! elem k (str v "px")))
  elem)

(defn set-attr!
  "Sets dom attributes on and returns `elem`.
   Attributes without values will be set to their name:

       (set-attr! elem :disabled)

   With values, the function takes variadic kv pairs:

       (set-attr! elem :id \"some-id\"
                       :name \"some-name\")"
  ([elem k] (set-attr! elem k (as-str k)))
  ([elem k v]
     (let [k (as-str k)]
       (when v
         (if (fn? v)
           (doto elem (aset k v))
           (doto elem (.setAttribute k v))))))
  ([elem k v & kvs]
     (assert (even? (count kvs)))
     (doseq [[k v] (->> kvs (partition 2) (cons [k v]))]
       (set-attr! elem k v))
     elem))

(defn remove-attr!
  "Removes dom attributes on and returns `elem`.
   `class` and `classes` are special cases which clear
   out the class name on removal."
  ([elem k]
     (let [k (as-str k)]
       (if (#{"class" "classes"} k)
         (set-class! elem "")
         (.removeAttribute elem k)))
     elem)
  ([elem k & ks]
     (doseq [k (cons k ks)]
       (remove-attr! elem k))
     elem))

(defn toggle-attr!
  "Toggles a dom attribute `k` on `elem`, optionally specifying
   the boolean value with `add?`"
  ([elem k]
     (toggle-attr! elem k (boolean (attr elem k))))
  ([elem k ^boolean add?]
     (if add?
       (set-attr! elem k)
       (remove-attr! elem k))))

(defn add-class!
  "Add `classes` to `elem`, trying to use Element::classList, and
   falling back to fast string parsing/manipulation"
  ([elem classes]
     (let [classes (-> classes as-str str/trim (.split #"\s+"))]
       (when (seq classes)
         (if-let [class-list (.-classList elem)]
           (doseq [c classes] (.add class-list c))
           (doseq [c classes]
             (let [class-name (class elem)]
               (when-not (utils/class-index class-name c)
                 (set-class! elem (if (identical? class-name "")
                                    c (str class-name " " c))))))))
       elem))
  ([elem classes & more-classes]
     (doseq [c (conj more-classes classes)]
       (add-class! elem c))
     elem))

(defn remove-class!
  "Remove `c` from `elem` class list"
  ([elem c]
     (let [c (as-str c)]
       (if-let [class-list (.-classList elem)]
         (.remove class-list c)
         (let [class-name (class elem)
               new-class-name (utils/remove-class-str class-name c)]
           (when-not (identical? class-name new-class-name)
             (set-class! elem new-class-name))))
       elem))
  ([elem class & classes]
     (doseq [c (conj classes class)]
       (remove-class! elem c))))

(defn toggle-class!
  "(toggle-class! elem class) will add-class! if elem does not have class
   and remove-class! otherwise.
   (toggle-class! elem class add?) will add-class! if add? is truthy,
   otherwise it will remove-class!"
  ([elem c]
     (let [c (as-str c)]
       (if-let [class-list (.-classList elem)]
         (.toggle class-list c)
         (toggle-class! elem c (not (has-class? elem c))))
       elem))
  ([elem class ^boolean add?]
     (if add?
       (add-class! elem class)
       (remove-class! elem class))
     elem))

(defn toggle!
  "Display or hide the given `elem` (using display: none).
   Takes an optional boolean `show?`"
  ([elem ^boolean show?]
     (set-style! elem :display (if show? "" "none")))
  ([elem] (toggle! elem (hidden? elem))))

(defn hide! [elem]
  (toggle! elem false))

(defn show! [elem] (toggle! elem true))

(defn scroll-into-view
  [elem ^boolean align-with-top?]
  (let [top (:top (bounding-client-rect elem))]
    (when (< js/window.innerHeight
             (+ top (.-offsetHeight elem)))
      (.scrollIntoView elem align-with-top?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOM Creation

(defn create-element
  ([tag]
     (.createElement js/document (as-str tag)))
  ([tag-ns tag]
     (.createElementNS
      js/document (as-str tag-ns) (as-str tag))))

(defn create-text-node
  [text]
  (.createTextNode js/document text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOM Manipulation

(defn clear!
  "Clears all children from `elem`"
  [elem]
  (set-html! elem ""))

(defn append!
  "Append `child` to `parent`"
  ([parent child]
     (doto parent
       (.appendChild child)))

  ([parent child & more-children]
     (doseq [c (cons child more-children)]
       (append! parent c))
     parent))

(defn prepend!
  "Prepend `child` to `parent`"
  ([parent child]
     (doto parent
       (.insertBefore child (.-firstChild parent))))

  ([parent child & more-children]
     (doseq [c (cons child more-children)]
       (prepend! parent c))
     parent))

(defn insert-before!
  "Insert `elem` before `other`, `other` must have a parent"
  [elem other]
  (let [p (parent other)]
    (assert p "Target element must have a parent")
    (.insertBefore p elem other)
    elem))

(defn insert-after!
  "Insert `elem` after `other`, `other` must have a parent"
  [elem other]
  (if-let [next (.-nextSibling other)]
    (insert-before! elem next)
    (append! (parent other) elem))
  elem)

(defn replace!
  "Replace `elem` with `new`, return `new`"
  [elem new]
  (let [p (parent elem)]
    (assert p "Target element must have a parent")
    (.replaceChild p new elem)
    new))

(defn replace-contents!
  "Replace children of `elem` with `child`"
  [p child]
  (append! (clear! p) child))

(defn remove!
  "Remove `elem` from `parent`, return `parent`"
  ([elem]
     (let [p (parent elem)]
       (assert p "Target element must have a parent")
       (remove! p elem)))

  ([p elem]
     (doto p (.removeChild elem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Events

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
    (let [selected-target (closest elem (.-target event) selector)]
      (when (and selected-target (not (attr selected-target :disabled)))
        (set! (.-selectedTarget event) selected-target)
        (f event)))))

(defn- event-listeners
  "Returns a nested map of event listeners on `elem`"
  [elem]
  (or (.-dommyEventListeners elem) {}))

(defn- update-event-listeners!
  [elem f & args]
  (let [elem elem]
    (set! (.-dommyEventListeners elem)
          (apply f (event-listeners elem) args))))

(defn- elem-and-selector
  [elem-sel]
  (if (sequential? elem-sel)
    ((juxt first rest) elem-sel)
    [elem-sel nil]))

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
  "Behaves like `listen!`, but removes the listener after the first event occurs."
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
