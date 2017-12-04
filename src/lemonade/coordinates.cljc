(ns lemonade.coordinates
  (:require [lemonade.core :as core]
            [lemonade.math :as math]))

(defn get-coord-inversion [height]
  (math/atx [1 0 0 -1] [0 height]))

(defn invert-coordinates [shape height]
  (core/transform shape (get-coord-inversion height)))

(defn wrap-invert-coordinates [render]
  (fn [state]
    (invert-coordinates (render state)
                        (-> state :lemonade.window/window :height))))
