(ns lemonade.examples.basic
  (:require [lemonade.core :as core :refer [annulus line polyline scale translate]]))

(def ex

  [(->> (assoc polyline :points [[0 0] [100 100] [300 100] [100 300] [0 0]])
        #_(translate [600 200])
        (scale 2)
        #_(rotate [600 200] 120))
   (assoc line :from [800 200] :to [900 200])
   (translate [500 500] #_(assoc core/circle :radius 300)
              (assoc annulus :outer-radius 300 :inner-radius 200
                     :style {:fill :red}))

   #_(scale 100 circle)])
