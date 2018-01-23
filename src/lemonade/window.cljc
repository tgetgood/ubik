(ns lemonade.window
  "Model the canvas as a window into R^2."
  (:require [lemonade.core :as core]
            [lemonade.math :as math]))

(defn normalise-zoom [dz]
  (let [scale 25]
    (math/exp (/ (- dz) scale))))

(defn zoom-c [dz ox zx]
  (let [dz (normalise-zoom dz)]
    (+ (* dz ox) (* zx (- 1 dz)))))

(defn update-zoom [{z :zoom o :offset :as w} zc dz]
  (assoc w
         :zoom (max -2000 (min 2000 (+ z dz)))
         :offset (mapv (partial zoom-c dz) o zc)))

(defn update-offset [w delta]
  (update w :offset #(mapv - % delta)))

(defn windowing-atx [{{:keys [zoom offset]} ::core/window}]
  (let [zoom (normalise-zoom zoom)]
    (math/comp-atx
     (core/translation offset)
     (core/scaling [zoom zoom]))))

(def initial-window
  {:zoom 0 :offset [0 0]})

(def window-events
  #:lemonade.events
  {:scroll    (fn [{:keys [dy location]}]
                {:mutation [update ::core/window update-zoom location dy]})

   :left-drag (fn [{:keys [delta]}]
                {:mutation [update ::core/window update-offset delta]})})

(defn wrap-windowing [render]
  (fn [state]
    (assoc
     (core/transform (render state) (windowing-atx state))
     :lemonade.events/handlers window-events)))
