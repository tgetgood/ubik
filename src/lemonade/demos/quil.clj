(ns lemonade.demos.quil
  (:require [clojure.pprint :refer [pprint]]
            [lemonade.examples.basic :as basic]
            [lemonade.renderers.quil :refer [render]]
            [quil.core :as q]))

(defn draw! []
  (q/fill 0 100 200)

  (q/clear)
  (q/background 255)
  (q/begin-shape)
  (q/line [100 100] [200 200])
  (q/line [200 200] [100 200])
  (q/line [100 200] [100 100])
  #_(render basic/ex2)
  (q/end-shape))

(defn sketch! []
   (q/defsketch quil-test
     :host "canvas"
     :renderer :p2d
     :features [:resizable]
     :draw draw!))
