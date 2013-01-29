(ns dommy.core
  (:use-macros [dommy.core-compile :only [sel]])
  (:require
   [clojure.string :as str]))

(defn append! [parent child]
  (.appendChild parent child))

(defn prepend! [parent child]
  (.insertBefore parent child (.-firstChild parent)))

(defn remove! [node]
  (.removeChild (.-parentNode node) node))

(defn replace! [node new]
  (.replaceChild (.-parentNode node) new node))

(defn selector [data]
  (cond
   (coll? data) (clojure.string/join " " (map selector data))
   (or (string? data) (keyword? data)) (name data)))

(defn listen!
  ([node event-type live-selector f]
     (listen! node event-type
        (fn [event]
          ;; does event.target match selector 
          (when (-> (sel node live-selector) (#(apply array %)) (.indexOf (.-target event)) (>= 0))
            (f event)))))
  ([node event-type f]
     (.addEventListener node (name event-type) f)))

(defn class-match? [class-name class i]
  (and
   ;; start
   (or (zero? i) (identical? \space (.charAt class-name (dec i))))
   ;; stop
   (let [total-len (.-length class-name)
         stop (+ i (.-length class))]
     (when (<= stop total-len)
       (or (identical? stop total-len)
           (identical? \space (.charAt class-name stop)))))))

(defn class-index
  "Finds the index of class in a space-delimited class-name"
  [class-name class]
  (loop [start-from 0]
    (let [i (.indexOf class-name class start-from)]
      (when (>= i 0)
        (if (class-match? class-name class i)
          i
          (recur (+ i (.-length class))))))))

(defn has-class? [node class]
  (when-let [i (class-index (.-className node) class)]
    (>= i 0)))

(defn add-class! [node c]
  (set! (.-className node)
        (let [cur-c (.-className node)]
          (if (or (identical? cur-c "") (> (.indexOf cur-c c) 0))
            c
            (str cur-c " " c)))))

(defn remove-class-str [init-class-name class]
  (loop [class-name init-class-name]
    (let [class-len (.-length class-name)]
      (if-let [i (class-index class-name class)]
        (recur (let [end (+ i (.-length class))]
                 (str (if (< end class-len)                        
                        (str (.substring class-name 0 i) (.substr class-name (inc end)))
                        (.substring class-name 0 (dec i))))))
        class-name))))

(defn remove-class! [node class]
  (let [class-name (.-className node)
        new-class-name (remove-class-str class-name class)]
    (when-not (identical? class-name new-class-name)
      (set! (.-className node) new-class-name))))

(defn toggle-class!
  ([node class]
     (toggle-class! node class (complement has-class?)))
  ([node class pred]
     (if (if (fn? pred) (pred class node) pred)
       (add-class! node class)
       (remove-class! node class))))