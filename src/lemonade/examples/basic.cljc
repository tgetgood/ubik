(ns lemonade.examples.basic
  (:require [lemonade.core :refer [line rotate scale translate]]))

(def ex
  (->> (assoc line :to [100 100])
       (translate [400 200])
       (scale [400 200] 2)
       (rotate [400 200] 120)))
