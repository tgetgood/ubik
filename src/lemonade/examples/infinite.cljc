(ns lemonade.examples.infinite
  "Rings of geometrically increasing radius. Infinite zoom and scroll while
  maintaining a limited render set."
  (:require [lemonade.core :refer [annulus circle scale]]
            [lemonade.math :as math]
            [lemonade.window :as window]))

(def factor (/ 36 64))

(def r 7)

(defn nth-ring [n]
  (-> annulus
      (assoc :style {:fill :rebeccapurple
                     :stroke :none}
             :inner-radius 6
             :outer-radius 8)
      (scale (math/exp factor (- n)))))

(defn contains-origin? [x y w h]
  (and (<= 0 x w) (<= 0 y h)))

(defn abs-dist [f x y w h]
  (math/sqrt (+ (math/exp (f (math/abs x)
                                     (math/abs (- w x))) 2)
                    (math/exp (f (math/abs y)
                                     (math/abs (- h y))) 2))))

(defn logit [n z]
  (math/floor (math/log (/ 1 factor) (/ n (* z r)))))

(defn rings [[w h] {:keys [zoom offset]}]
  (let [z (window/normalise-zoom zoom)
        [x y] offset]
    (let [M (abs-dist max x y w h)
          N (+ 2 (logit M z))
          m 1
          n (logit m z)]
      (map nth-ring (range n N)))))


(defn example [size-fn]
  (fn [state]
    (if-let [window (::window/window state)]
      (rings (size-fn) window)
      [])))
