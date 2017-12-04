(ns lemonade.events
  (:require [lemonade.core :as core]
            [lemonade.math :as math]
            [lemonade.system :as system]))

(defn- handle
  "Check if event applies to shape, and if so deal with it."
  [shape event]
  (when-let [event-map (::handlers shape)]
    (when (contains? event-map (:type event))
      ((get event-map (:type event)) event))))

(defmulti ^:private event-traversal-walk
  (fn [shape event] (core/classify shape)))

(defn- event-traversal [shape event]
  (let [res (handle shape event)]
    (doseq [ev (:dispatches res)]
      (event-traversal-walk shape ev))
    (when-let [ev (:dispatch res)]
      (event-traversal shape ev))
    (when-let [mutation (:mutation res)]
      (system/handle-mutation mutation))
    (when-not (:stop res)
      (event-traversal-walk shape event))))

(defmethod event-traversal-walk :default
  [_ _ _]
  nil)

(defmethod event-traversal-walk ::core/sequential
  [shape event]
  (mapv #(event-traversal % event) shape))

(defmethod event-traversal-walk ::core/composite
  [shape event]
  (event-traversal (:contents shape) event))

(defmethod event-traversal-walk ::core/path
  [shape event]
  (event-traversal (:contents shape) event))

(defmethod event-traversal-walk ::core/atx
  [shape event]
  (if (contains? event :location)
    (let [inv (math/invert-atx (:atx shape))]
      (event-traversal (:base-shape shape)
                       (update event :location #(math/apply-atx inv %))))
    (event-traversal (:base-shape shape) event)))

;; Just ignore events issued before the system initialises.
(defn dispatch!
  "Dispatch an event from the very top. Meant for integration with low level
  event systems like processing graphics or the dom."
  [ev]
  (event-traversal (system/world) ev))

;; We're no longer using this for window, since window needs to be built into
;; the system to work properly.
;;
;; REVIEW: Is there a valid use case for this at all?
;; After all, user defined handlers can be initialised in the app state which is
;; user controlled. What about library handlers?
(defn init-event-handlers! []
  #?(:clj (throw (Exception. "Not Implemented"))
     ;; Invoke on the first animation frame after something has rendered.
     :cljs (letfn [(recurrent []
                     (if (system/world)
                       (dispatch! {:type ::init})
                       (js/window.requestAnimationFrame recurrent)))]
             (recurrent))))
