(ns lemonade.events
  (:require [lemonade.core :as core]
            [lemonade.geometry :as geometry]))

;; FIXME: This is very side effectful, and I don't think sufficiently isolated
;; from the rest of the code.
(defn handle [shape event]
  (when-let [event-map (::handlers shape)]
    (when (contains? event-map (:type event))
      (= ::stop ((get event-map (:type event)) event)))))

(defmulti ^:private event-traversal*
  (fn [shape event] (core/classify shape)))

(defn event-traversal [shape event]
  (when-not (handle shape event)
    (event-traversal* shape event)))

(defmethod event-traversal* :default
  [_ _]
  nil)

(defmethod event-traversal* ::core/sequential
  [shape event]
  (mapv #(event-traversal % event) shape))

(defmethod event-traversal* ::core/atx
  [shape event]
  (if (contains? event :location)
    (let [inv (geometry/invert-atx (:atx shape))]
      (event-traversal (:base-shape shape)
                       (update event :location #(geometry/apply-atx inv %))))
    (event-traversal (:base-shape shape) event)))

(defn fire! [event]
  )
