(ns lemonade.examples.basic
  (:require [lemonade.core :refer [polyline rotate scale translate] :as core]))

(def ex
  (->> (assoc polyline :points [[0 0] [100 100] [300 100] [100 300] [0 0]])
       (translate [600 200])
       (scale [600 200] 2)
       #_(rotate [600 200] 120)))
