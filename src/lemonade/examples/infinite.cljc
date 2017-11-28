(ns lemonade.examples.infinite
  (:require [lemonade.core :refer [scale translate]]
            [lemonade.examples.elections :refer [ring-example]]))

(defn example [state]
  [ring-example
   (-> ring-example
       (scale (/ 3125 32768)))
   (-> ring-example
       (scale (/ 3125 32768))
       (scale (/ 3125 32768)))
   (-> ring-example
       (scale (/ 3125 32768))
       (scale (/ 3125 32768))
       (scale (/ 3125 32768)))
   (-> ring-example
       (scale (/ 3125 32768))
       (scale (/ 3125 32768))
       (scale (/ 3125 32768))
       (scale (/ 3125 32768)))
   (-> ring-example
       (scale (/ 3125 32768))
       (scale (/ 3125 32768))
       (scale (/ 3125 32768))
       (scale (/ 3125 32768))
       (scale (/ 3125 32768)))])
