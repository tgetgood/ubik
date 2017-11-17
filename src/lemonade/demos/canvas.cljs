(ns lemonade.demos.canvas
  (:require [lemonade.core :as core]
            [lemonade.events.canvas :as dom-events]
            [lemonade.events.core :as events]
            [lemonade.examples.elections :as elections]
            [lemonade.geometry :as geometry]
            [lemonade.renderers.canvas :as rc]
            [lemonade.window :as window]))

;; Setup

(enable-console-print!)

;;;;; Canvas Element handling

(defn canvas-elem []
  (.getElementById js/document "canvas"))

(defn canvas-container []
  (.getElementById js/document "canvas-container"))

(defn canvas-container-dimensions []
  (let [cc (canvas-container)]
    [(.-clientWidth cc) (.-clientHeight cc)]))

(defn set-canvas-size! [canvas [width height]]
  (set! (.-width canvas) width)
  (set! (.-height canvas) height))

(defn canvas-container-offset []
  (let [c (canvas-container)]
    [(.-offsetLeft c) (.-offsetTop c)]))

(defn fullscreen-canvas! []
  (let [[w h :as dim] (canvas-container-dimensions)]
    (set-canvas-size! (canvas-elem) dim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-coord-inversion [elem]
  (geometry/atx [1 0 0 -1] [0 (.-height elem)]))

(defn wrap-invert-coordinates [render elem]
  (fn [state]
    (core/transform (render state) (get-coord-inversion elem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce state
  (atom {:election-data  elections/election-data
         :interactive []}))

(defn prerender
  "Main render fn."
  [{:keys [window election-data interactive]}]
  [(elections/election election-data)
   interactive])

;; ideal scenario
(def handler
  (let [elem (canvas-elem)]
    (-> prerender
        window/wrap-windowing
        (wrap-invert-coordinates elem))))

(defn on-js-reload []
  (fullscreen-canvas!)

  (let [elem (canvas-elem)]
    (dom-events/init-event-system! elem)

    ;; TODO: Somehow set up the lemonade event system.
    (events/clear-events!)
    (events/register-event-handlers (window/window-events state) ::window/events)

    (core/draw-loop state handler (partial rc/draw! elem) false)))

(defn ^:export init []
  (on-js-reload)
  ;; Init app state just once.
  (events/fire! {:type :lemonade.events/init}))
