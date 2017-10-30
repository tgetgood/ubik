(ns lemonade.demos.quil
  (:require [quil.core :as q]
            [lemonade.renderers.quil :refer [renderer]]
            [lemonade.examples.basic :as basic]))

(defn draw! []
  (q/clear)
  (q/background 255)
  (q/reset-matrix)
  (let [r (renderer basic/ex)]
    (r)))

(defn sketch! [w h]
   (q/defsketch quil-test
     :host "canvas"
     :renderer :p2d
     :size [w h]
     :draw draw!))
