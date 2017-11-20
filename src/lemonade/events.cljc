(ns lemonade.events
  (:require [lemonade.core :as core]
            [lemonade.geometry :as geometry]))

;; FIXME: This is very side effectful, and I don't think sufficiently isolated
;; from the rest of the code.
(defn- handle [shape state-ref event]
  (when-let [event-map (::handlers shape)]
    (when (contains? event-map (:type event))
      (= ::stop ((get event-map (:type event)) state-ref event)))))

(defmulti ^:private event-traversal*
  (fn [shape state-ref event] (core/classify shape)))

(defn- event-traversal [shape state-ref event]
  (when-not (handle shape state-ref event)
    (event-traversal* shape state-ref event)))

(defmethod event-traversal* :default
  [_ _ _]
  nil)

(defmethod event-traversal* ::core/sequential
  [shape state-ref event]
  (mapv #(event-traversal % state-ref event) shape))

(defmethod event-traversal* ::core/atx
  [shape state-ref event]
  (if (contains? event :location)
    (let [inv (geometry/invert-atx (:atx shape))]
      (event-traversal (:base-shape shape)
                       state-ref
                       (update event :location #(geometry/apply-atx inv %))))
    (event-traversal (:base-shape shape) state-ref event)))

(defn shape-traversing-event-handler [state]
  (fn [ev]
    (event-traversal (:lemonade.core/world @state) state ev)))

(defn init-event-handlers! [state]
  #?(:clj (throw (Exception. "Not Implemented"))
     ;; Invoke on the first animation frame after something has rendered.
     :cljs (letfn [(recurrent []
                     (if (contains? @state ::core/world)
                       ((shape-traversing-event-handler state) {:type ::init})
                       (js/window.requestAnimationFrame recurrent)))]
             (recurrent))))
