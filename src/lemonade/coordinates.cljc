(ns lemonade.coordinates
  (:require [lemonade.core :as core]
            [lemonade.math :as math]))

(defn get-coord-inversion [canvas]
  #?(:cljs (math/atx [1 0 0 -1] [0 (.-height canvas)])))

(defn invert-coordinates [shape elem]
  (core/transform shape (get-coord-inversion elem)))

(defn wrap-invert-coordinates [render elem]
  (fn [state]
    (invert-coordinates (render state) elem)))

(defn coord-inversion-export [canvas]
  {:type :atx
   :atx-fn (constantly (get-coord-inversion canvas))})
