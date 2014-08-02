(ns dommy.template
  (:require
   [clojure.string :as str]
   [dommy.attrs :as attrs]
   [dommy.utils :refer [as-str]]))

(def +svg-ns+ "http://www.w3.org/2000/svg")
(def +svg-tags+ #{"svg" "g" "rect" "circle" "clipPath" "path" "line" "polygon" "polyline" "text" "textPath"})

(defprotocol PElement
  (-elem [this] "return the element representation of this"))

(defn next-css-index
  "index of css character (#,.) in base-element. bottleneck"
  [s start-idx]
  (let [id-idx (.indexOf s "#" start-idx)
        class-idx (.indexOf s "." start-idx)
        idx (.min js/Math id-idx class-idx)]
    (if (< idx 0)
      (.max js/Math id-idx class-idx)
      idx)))

(defn base-element
  "dom element from css-style keyword like :a.class1 or :span#my-span.class"
  [node-key]
  (let [node-str (as-str node-key)
        base-idx (next-css-index node-str 0)
        tag (cond
             (> base-idx 0) (.substring node-str 0 base-idx)
             (zero? base-idx) "div"
             :else node-str)
        node (if (+svg-tags+ tag)
               (.createElementNS js/document +svg-ns+ tag)
               (.createElement js/document tag))]
    (when (>= base-idx 0)
      (loop [str (.substring node-str base-idx)]
        (let [next-idx (next-css-index str 1)
              frag (if (>= next-idx 0)
                     (.substring str 0 next-idx)
                     str)]
          (case (.charAt frag 0)
            \. (attrs/add-class! node (.substring frag 1))
            \# (.setAttribute node "id" (.substring frag 1)))
          (when (>= next-idx 0)
            (recur (.substring str next-idx))))))
    node))

(defn throw-unable-to-make-node [node-data]
  (throw (str "Don't know how to make node from: " (pr-str node-data))))

(defn ->document-fragment
  "take data and return a document fragment"
  ([data]
     (->document-fragment (.createDocumentFragment js/document) data))
  ([result-frag data]
     (cond
      (satisfies? PElement data)
      (do (.appendChild result-frag (-elem data))
          result-frag)

      (seq? data)
      (do (doseq [child data] (->document-fragment result-frag child))
          result-frag)

      (nil? data)
      result-frag

      :else
      (throw-unable-to-make-node data))))

(defn ->node-like
  "take data and return DOM node if it satisfies PElement and tries to
   make a document fragment otherwise"
  [data]
  (if (satisfies? PElement data)
    (-elem data)
    (->document-fragment data)))

(defn compound-element
  "element with either attrs or nested children [:div [:span \"Hello\"]]"
  [[tag-name maybe-attrs & children]]
  (let [n (base-element tag-name)
        attrs (when (and (map? maybe-attrs)
                         (not (satisfies? PElement maybe-attrs)))
                maybe-attrs)
        children  (if attrs children (cons maybe-attrs children))]
    (doseq [[k v] attrs]
      (case k
        :class (attrs/add-class! n v)
        :classes (doseq [c v] (attrs/add-class! n c))
        (attrs/set-attr! n k v)))
    (.appendChild n (->node-like children))
    n))

(extend-protocol PElement
  js/Element
  (-elem [this] this)

  js/Comment
  (-elem [this] this)

  js/Text
  (-elem [this] this)

  PersistentVector
  (-elem [this] (compound-element this))

  number
  (-elem [this] (.createTextNode js/document (str this)))

  string
  (-elem [this]
    (if (keyword? this)
      (base-element this)
      (.createTextNode js/document (str this)))))

;; extend additional prototypes, which might not be available on all
;; versions of IE or phantom

(when (exists? js/HTMLElement)
  (extend-protocol PElement
    js/HTMLElement
    (-elem [this] this)))

(when (exists? js/DocumentFragment)
  (extend-protocol PElement
    js/DocumentFragment
    (-elem [this] this)))

(when (exists? js/Document)
  (extend-protocol PElement
    js/Document
    (-elem [this] this)))

(when (exists? js/HTMLDocument)
  (extend-protocol PElement
    js/HTMLDocument
    (-elem [this] this)))

(when (exists? js/SVGElement)
  (extend-protocol PElement
    js/SVGElement
    (-elem [this] this)))

(when (exists? js/Window)
  (extend-protocol PElement
    js/Window
    (-elem [this] this)))

(defn node [data]
  (if (satisfies? PElement data)
    (-elem data)
    (throw-unable-to-make-node data)))

(defn html->nodes [html]
  (let [parent (.createElement js/document "div")]
    (.insertAdjacentHTML parent "beforeend" html)
    (->> parent .-childNodes (.call js/Array.prototype.slice) seq)))
