(ns lemonade.events
  (:require [lemonade.core :as core]
            [lemonade.geometry :as geometry]))

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
    (doseq [ev (::dispatch! res)]
      (println ev)
      (event-traversal-walk shape ev))
    (when-not (::stop res)
      (event-traversal-walk shape event))))

(defmethod event-traversal-walk :default
  [_ _ _]
  nil)

(defmethod event-traversal-walk ::core/sequential
  [shape event]
  (mapv #(event-traversal % event) shape))

(defmethod event-traversal-walk ::core/composite
  [shape event]
  (mapv #(event-traversal % event) (:contents shape)))

(defmethod event-traversal-walk ::core/path
  [shape event]
  (mapv #(event-traversal % event) (:contents shape)))

(defmethod event-traversal-walk ::core/atx
  [shape event]
  (if (contains? event :location)
    (let [inv (geometry/invert-atx (:atx shape))]
      (event-traversal (:base-shape shape)
                       (update event :location #(geometry/apply-atx inv %))))
    (event-traversal (:base-shape shape) event)))

(defn- shape-traversing-event-handler [state]
  (fn [ev]
    (event-traversal (:lemonade.core/world @state) ev)))

;; HACK: This method of setting up the event queue isn't going to last.

(defonce ^:private watcher (atom nil))

;; Just ignore events issued before the system initialises.
(defn dispatch! [ev]
  (let [cb @watcher]
    (when (fn? cb)
      (cb ev))))

(defn init-event-handlers! [state]
  #?(:clj (throw (Exception. "Not Implemented"))
     ;; Invoke on the first animation frame after something has rendered.
     :cljs (letfn [(recurrent []
                     (if (contains? @state ::core/world)
                       (do
                         (reset! watcher (shape-traversing-event-handler state))
                         (dispatch! {:type ::init}))
                       (js/window.requestAnimationFrame recurrent)))]
             (recurrent))))
