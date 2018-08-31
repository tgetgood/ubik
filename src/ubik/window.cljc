(ns ubik.window
  "Model the canvas as a window into R^2."
  (:require [ubik.core :as core]
            [ubik.math :as math]))

(defn normalise-zoom [dz]
  (let [scale 100]
    (math/exp (/ (- dz) scale))))

(defn zoom-c [dz ox zx]
  (let [dz (normalise-zoom dz)]
    (+ (* dz ox) (* zx (- 1 dz)))))

(defn update-zoom [{z :zoom o :offset :as w} zc dz]
  (assoc w
         :zoom (max -8000 (min 8000 (+ z dz)))
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
  #:ubik.events
  {:scroll    (fn [{:keys [dy location]} _ _]
                {:mutation [update ::core/window update-zoom location dy]})

   :left-drag (fn [{:keys [delta]} _ _]
                {:mutation [update ::core/window update-offset delta]})})

(defn wrap-windowing [render]
  (fn [state]
    (let [{:keys [height width]} (:ubik.core/window state)]
      [(with-meta (assoc core/frame :height height :width width)
         {:events {:key ::window}})
       (with-meta
         (core/transform (render state) (windowing-atx state))
         {:atx-type ::core/window})])))
