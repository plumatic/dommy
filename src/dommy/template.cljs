(ns dommy.template
  (:require [clojure.string :as str]
            [dommy.core :as dommy]))

(defprotocol PElement
  (-elem [this] "return the element representation of this"))

(defn style-str [m]
  (->> m
       (map (fn [[k v]] (str (name k) ":" (name v) ";")))
       (str/join " ")))

(defn add-attr! 
  "can have a seq for :classes key or a map for :style"
  [node k v]
  (when v 
    (case k
      :class (dommy/add-class! node v)
      :classes (doseq [c v] (dommy/add-class! node c))
      :style (.setAttribute node (name k) (style-str v))
      (.setAttribute node (name k) v)))) 

(defn next-css-index [s start-idx]
  "index of css character (#,.) in base-element. bottleneck"
  (let [id-idx (.indexOf s "#" start-idx)
        class-idx (.indexOf s "." start-idx)
        idx (.min js/Math id-idx class-idx)]
    (if (< idx 0)
      (.max js/Math id-idx class-idx)
      idx)))

(defn base-element
  "dom element from css-style keyword like :a.class1 or :span#my-span.class"
  [node-key]
  (let [node-str (name node-key)
        base-idx (next-css-index node-str 0)
        tag (cond
              (> base-idx 0) (.substring node-str 0 base-idx)
              (zero? base-idx) "div"
              :else node-str)
        node (.createElement js/document tag)]
    (when (>= base-idx 0)
      (loop [str (.substring node-str base-idx)]
        (let [next-idx (next-css-index str 1)
              frag (if (>= next-idx 0)
                     (.substring str 0 next-idx)
                     str)]
          (case (.charAt frag 0)
            \. (dommy/add-class! node (.substring frag 1))
            \# (.setAttribute node "id" (.substring frag 1)))
          (when (>= next-idx 0)
            (recur (.substring str next-idx))))))
    node))

(declare node)

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
  [data]
  (let [n (base-element (first data))
        attrs     (when (map? (second data)) (second data))
        children  (drop (if attrs 2 1) data)]
    (doseq [[k v] attrs]
      (add-attr! n k v))
    (.appendChild n (->node-like children))
    n))

(extend-protocol PElement
  js/HTMLElement
  (-elem [this] this)

  PersistentVector
  (-elem [this] (compound-element this))

  js/Text
  (-elem [this] this)

  number
  (-elem [this] (.createTextNode js/document (str this)))

  string
  (-elem [this]
         (if (keyword? this)
           (base-element this)
           (.createTextNode js/document (str this)))))

(defn node [data]
  (if (satisfies? PElement data)
    (-elem data)
    (throw-unable-to-make-node data)))

(defn html->nodes [html]
  (let [parent (.createElement js/document "div")]
    (.insertAdjacentHTML parent "beforeend" html)
    (->> parent .-childNodes (.call js/Array.prototype.slice))))
