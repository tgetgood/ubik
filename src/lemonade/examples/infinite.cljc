(ns lemonade.examples.infinite
  (:require [lemonade.core :refer [annulus scale]]
            [lemonade.examples.elections :refer [ring-example]]
            [lemonade.geometry :as geometry]
            [lemonade.window :as window]))

(def factor (/ 36 64))

(def r 7)

(def simple-ring
  (assoc annulus
         :inner-radius 6
         :outer-radius 8
         :style {:fill :rebeccapurple
                 :stroke :none}))

(defn nth-ring [n]
  (-> simple-ring
      (scale (geometry/exp factor (- n)))))

(defn contains-origin? [[x y] w h]
  (and (<= 0 x w) (<= 0 y h)))

(defn rings [[w h] {:keys [zoom offset]}]
  (let [z (window/normalise-zoom zoom)
        [x y] offset]
    (let [m (geometry/sqrt (+ (geometry/exp (max x (+ x w)) 2)
                              (geometry/exp (max y (+ y h)) 2)))
          N (geometry/floor (geometry/log (/ 1 factor) (/ m (* z r))))
          n (inc (geometry/floor (geometry/log (/ 1 factor) (/ 1 (* z r)))))]
      (println n N z)
      [(map nth-ring (range n N))
       (nth-ring 160)]
      )))


(defn example [size-fn]
  (fn [state]
    (if-let [window (::window/window state)]
      (rings (size-fn) window)
      [])))
