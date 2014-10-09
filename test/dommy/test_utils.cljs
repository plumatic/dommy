(ns dommy.test-utils
  (:require
   [dommy.core :as dommy]))

(defn fire!
  "Creates an event of type `event-type`, optionally having
   `update-event!` mutate and return an updated event object,
   and fires it on `node`.
   Only works when `node` is in the DOM"
  [node event-type & [update-event!]]
  (let [update-event! (or update-event! identity)]
    (if (.-createEvent js/document)
      (let [event (.createEvent js/document "Event")]
        (.initEvent event (name event-type) true true)
        (.dispatchEvent node (update-event! event)))
      (.fireEvent node (str "on" (name event-type))
                  (update-event! (.createEventObject js/document))))))

(defn ce [tag & [text]]
  (let [el (dommy/create-element tag)]
    (when text
      (dommy/set-text! el text))
    el))

(defn el-tree []
  (let [grandchild (-> (ce :div) (dommy/add-class! :grandchild))
        child (-> (ce :div)
                  (dommy/add-class! :child)
                  (dommy/append! grandchild))
        parent (-> (ce :div)
                   (dommy/add-class! :parent)
                   (dommy/append! child))]
    [grandchild child parent]))
