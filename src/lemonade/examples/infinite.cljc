(ns lemonade.examples.infinite
  (:require [lemonade.core :refer [scale]]
            [lemonade.examples.elections :refer [ring-example]]
            [lemonade.geometry :as geometry]
            [lemonade.window :as window]))

(def factor (/ 3125 32768))

(defn nth-ring [n]
  (-> ring-example
      (scale (geometry/exp factor (- n)))))

(defn contains-origin? [[x y] w h]
  (and (<= 0 x w) (<= 0 y h)))

(defn rings [w h {:keys [zoom offset]}]
  (let [z       (window/normalise-zoom zoom)
        [x y]   (map (partial * (/ 1 z)) offset)
        [x' y'] (map - [x y] (map (partial * (/ 1 z)) [w h]))]
    (if (contains-origin? offset w h)
      (nth-ring 2)
      [])))


(defn example [w h]
  (fn [state]
    (if-let [window (::window/window state)]
      (rings w h window)
      [])))
