(ns lemonade.examples.basic
  (:require [lemonade.core :as core
             :refer [annulus line polyline rotate scale translate]]))

(def ex

  [(->> (assoc polyline
               :points [[0 0] [100 100] [300 100] [100 300] [0 0]]
               :style {:stroke {:corners :square
                                :colour :cyan
                                }
                       :fill :purple})
        (scale 3)
        #_(rotate  20)
        (translate [10 10]))
   (assoc line :from [800 100] :to [900 100])
   (core/with-style {:fill :pink
                     :stroke {:colour :blue
                              :dash []
                              :width  5}}
     (translate [500 500]
                (assoc annulus :outer-radius 300 :inner-radius 200
                       :style {:fill   :red
                               :dash   [5 5]
                               :stroke :pink}
                       )))

   (scale [1000 500] core/circle)])
