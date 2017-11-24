(ns lemonade.window
  "Model the canvas as a window into R^2."
  (:require [lemonade.core :as core]
            [lemonade.events :as events]
            [lemonade.geometry :as geometry]))

(defn normalise-zoom [dz]
  (let [scale 100]
    (geometry/exp (/ (- dz) scale))))

(defn zoom-c [dz ox zx]
  (let [dz (normalise-zoom dz)]
    (+ (* dz ox) (* zx (- 1 dz)))))

(defn update-zoom [{z :zoom o :offset :as w} zc dz]
  (assoc w
         :zoom (+ z dz)
         :offset (mapv (partial zoom-c dz) o zc)))

(defn update-offset [w delta]
  (update w :offset #(mapv - % delta)))

(defn windowing-atx [{{:keys [zoom offset]} ::window}]
  (let [zoom (normalise-zoom zoom)]
    (geometry/comp-atx
     (core/translation offset)
     (core/scaling [zoom zoom]))))

(def window-events
  #::events
  {:init      (fn [state _]
                (swap! state assoc ::window {:zoom 0 :offset [0 0]}))

   :scroll    (fn [state {:keys [dy location]}]
                (swap! state update ::window update-zoom location dy))

   :left-drag (fn [state {:keys [delta]}]
                (swap! state update ::window update-offset delta))})

(defn wrap-windowing [render]
  (fn [state]
    (assoc
     (core/transform (render state) (windowing-atx state))
     :lemonade.events/handlers window-events)))
