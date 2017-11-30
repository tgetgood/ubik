(ns lemonade.examples.infinite
  "Rings of geometrically increasing radius. Infinite zoom and scroll while
  maintaining a limited render set."
  (:require [lemonade.core :refer [annulus circle scale]]
            [lemonade.geometry :as geometry]
            [lemonade.window :as window]))

(def factor (/ 36 64))

(def r 7)

(defn nth-ring [n]
  (-> annulus
      (assoc :style {:fill :rebeccapurple
                     :stroke :none}
             :inner-radius 6
             :outer-radius 8)
      (scale (geometry/exp factor (- n)))))

(defn contains-origin? [x y w h]
  (and (<= 0 x w) (<= 0 y h)))

(defn abs-dist [f x y w h]
  (geometry/sqrt (+ (geometry/exp (f (geometry/abs x)
                                     (geometry/abs (- w x))) 2)
                    (geometry/exp (f (geometry/abs y)
                                     (geometry/abs (- h y))) 2))))

(defn logit [n z]
  (geometry/floor (geometry/log (/ 1 factor) (/ n (* z r)))))

(defn rings [[w h] {:keys [zoom offset]}]
  (let [z (window/normalise-zoom zoom)
        [x y] offset]
    (let [M (abs-dist max x y w h)
          N (+ 2 (logit M z))
          m 1
          n (logit m z)]
      (println n N)
      (map nth-ring (range n N)))))


(defn example [size-fn]
  (fn [state]
    (if-let [window (::window/window state)]
      (rings (size-fn) window)
      [])))
