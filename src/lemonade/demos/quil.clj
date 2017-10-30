(ns lemonade.demos.quil
  (:require [quil.core :as q
            [lemonade.renderers.quil :refer [renderer]]
            [lemonade.examples.basic :as basic]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 3)
  (q/background 255)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  )

(defn draw! []
  #?(:clj (q/clear))
  (q/background 255)
  (q/reset-matrix)
  (let [r (renderer basic/ex)]
    (r)))

(defn sketch! [w h]
   (q/defsketch quil-test
     :host "canvas"
     :renderer :p2d
     :size [w h]
     ;; setup function called only once, during sketch initialization.
     :setup setup
     ;; update-state is called on each iteration before draw-state.
     :draw draw!
     ;; This sketch uses functional-mode middleware.
     ;; Check quil wiki for more info about middlewares and particularly
     ;; fun-mode.
))
