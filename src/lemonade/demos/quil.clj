(ns lemonade.demos.quil
  (:require [clojure.pprint :refer [pprint pp]]
            [lemonade.renderers.quil :refer [renderer]]
            [quil.core :as q]))

(defn draw! []
  (q/fill 0 100 200)

  (q/clear)
  (q/background 255)
  (q/begin-shape)
  (q/line [100 100] [200 200])
  (q/line [200 200] [100 200])
  (q/line [100 200] [100 100])
  (q/end-shape))

(defn sketch! []
   (q/defsketch quil-test
     :host "canvas"
     :renderer :p2d
     :features [:resizable]
     :draw draw!))
