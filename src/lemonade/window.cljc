(ns lemonade.window
  "Model the canvas as a window into R^2."
  (:require [lemonade.core :as core]
            [lemonade.events :as events]
            [lemonade.geometry :as geometry]))

(defn normalise-zoom [dz]
  (let [scale 100]
    ;; TODO: Compare result with expm1.
    (geometry/exp (/ (- dz) scale))))

(defn zoom-c [dz ox zx]
  (+ (* dz ox) (* zx (- 1 dz))))

(defn update-zoom [{z :zoom o :offset :as w} zc dz]
  (assoc w
         :zoom (+ z dz)
         :offset (mapv (partial zoom-c dz) o zc)))

(defn update-offset [{:keys [zoom] :as w} [dx dy]]
  (update w :offset
          (fn [[x y]]
            [(- x dx) (- y dy)])))

(defn windowing-atx [{{:keys [zoom offset]} ::window}]
  (let [zoom (normalise-zoom zoom)]
    (geometry/comp-atx
     (core/translation offset)
     (core/scaling [zoom zoom]))))

(def window-events
  ;; REVIEW: These guys are going to mutatate state which has to be an
  ;; atom. Something like re-frame or javelin with a signal graph could help a
  ;; lot here.
  ;;
  ;; REVIEW: Cool. I've separated the event logic for the windowing into an
  ;; infinite canvas from the browser. I think,..
  (let [drag-state (atom nil)]
    #::events
    {:init            (fn [state]
                        (swap! state assoc ::window {:zoom 0 :offset [0 0]}))

     :wheel           (fn [state {:keys [dy location]}]
                        (swap! state update ::window update-zoom location dy))

     :left-mouse-down (fn [_ {:keys [location]}]
                        (reset! drag-state location))

     :mouse-move      (fn [state {:keys [location]}]
                        (when @drag-state
                          (let [delta (map - location @drag-state)]
                            (reset! drag-state location)
                            (swap! state update ::window delta))))

     :left-mouse-up   (fn [_ _]
                        (reset! drag-state nil))}))
