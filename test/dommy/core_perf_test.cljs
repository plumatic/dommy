(ns dommy.core-perf-test
  (:use-macros
   [dommy.macros :only [node sel sel1]])
  (:require
   [dommy.core :as dommy]))

(defn run-test [[key sel-fn]]
  (let [now (js/Date.)]
    (dotimes [n 1e5] (sel-fn))
    [key (/ (- (js/Date.) now) 1000)]))

(defn ^:export selector-perf-test []
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
       (map run-test)
       (into {})
       str))