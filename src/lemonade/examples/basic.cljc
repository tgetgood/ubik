(ns lemonade.examples.basic
  (:require [lemonade.core :as core
             :refer [annulus line polyline rotate scale translate]]))

(def ex
  [(-> polyline
       (assoc :points [[0 0] [100 100] [300 100] [100 300] [0 0]]
              :style {:stroke {:corners :square
                               :colour  :cyan}
                      :fill   :purple})
       (scale 3)
       (rotate 20)
       (translate [300 40]))
   (assoc line :from [800 100] :to [900 100])
   (core/with-style {:fill   :pink
                     :stroke {:colour :blue}}
     (-> annulus
         (assoc :outer-radius 300
                :inner-radius 200
                :style {:fill   :red

                        :stroke :pink})
         (translate [500 500])))

   (scale core/circle [4000 500])])


(def ex2
  [(assoc line :from [100 100] :to [200 200])
   (assoc line :from [200 200] :to [100 200])
   (assoc line :from [100 200] :to [100 100])])
