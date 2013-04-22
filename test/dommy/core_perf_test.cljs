(ns dommy.core-perf-test
  (:use-macros
   [dommy.macros :only [node sel sel1]])
  (:require
   [dommy.core :as dommy]))

(defn profile-fn [f]
  (let [now (js/performance.now)]
    (f)
    (- (js/performance.now) now)))

(defn ^:export selector-perf-test [samples]
  (dommy/append! js/document.body [:#c1 [:.c2 [:.c3]] [:.c3]])
  (->> {:dommy-body #(sel1 :body)
        :jquery-body #(js/jQuery "body")
        :dommy-id #(sel1 :#c1)
        :jquery-id #(js/jQuery "#c1")
        :dommy-class-sel1 #(sel1 :.c3)
        :jquery-class-sel1 #(aget (js/jQuery ".c3") 0)
        :dommy-multi-class #(sel ".c2, .c3")
        :jquery-multi-class #(js/jQuery ".c2, .c3")}
       shuffle
       (map (fn [[k f]]
              (let [results (repeatedly samples #(profile-fn f))
                    total (apply + results)]
                [k {:total total
                    :mean (/ total (count results))}])))
       (into {})
       clj->js))