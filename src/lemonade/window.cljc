(ns lemonade.window
  "Model the canvas as a window into R^2."
  (:require [lemonade.core :as core]
            [lemonade.geometry :as geometry]))

(defn normalise-zoom [dz]
  (let [scale 100]
    ;; TODO: Compare result with expm1.
    (geometry/exp (/ (- dz) scale))))

(defn zoom-c [dz ox zx]
  (+ (* dz ox) (* zx (- 1 dz))))

(defn update-zoom [{z :zoom o :offset :as w} zc dz]
  (assoc w
         :zoom (* z dz)
         :offset (mapv (partial zoom-c dz) o zc)))

(defn update-offset [{:keys [zoom] :as w} [dx dy]]
  (update w :offset
          (fn [[x y]]
            [(- x dx) (- y dy)])))

(defn windowing-atx [{:keys [zoom offset]}]
  (geometry/comp-atx
   (core/translation offset)
   (core/scaling [zoom zoom])))
