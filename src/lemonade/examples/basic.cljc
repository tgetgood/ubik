(ns lemonade.examples.basic
  (:require [lemonade.core :refer [line rotate translate]]))

(def ex
  (->> (assoc line :to [100 100])
       (translate [400 200])
       (rotate  20)))
