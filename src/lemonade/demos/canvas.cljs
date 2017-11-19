(ns lemonade.demos.canvas
  (:require [lemonade.coordinates :as coords]
            [lemonade.core :as core]
            [lemonade.events.canvas :as dom-events]
            [lemonade.events.core :as events]
            [lemonade.examples.elections :as elections]
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
;;;;; Export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce state
  (atom {:election-data elections/election-data
         :interactive   []}))

(defn base
  "Main render fn."
  [{:keys [window election-data interactive]}]
  [(elections/election election-data)
   interactive])

(defn interactive-hud [render]
  (fn [state]
    [(render state) (assoc core/circle :radius 200
                           :style {:fill :yellow
                                   :opacity 0.4
                                   :stroke :none})]))

;; ideal scenario
(def handler
  (let [elem (canvas-elem)]
    (-> base
        window/wrap-windowing
        interactive-hud
        (coords/wrap-invert-coordinates elem))))

(defn on-js-reload []
  (fullscreen-canvas!)

  (let [elem (canvas-elem)]
    (core/initialise!
     {:event-system (dom-events/event-system elem)
      :render       (partial rc/draw! elem)
      :handler      handler
      :app-db       state})

    ;; TODO: Somehow set up the lemonade event system.
    (events/clear-events!)
    (events/register-event-handlers (window/window-events state) ::window/events)))

(defn ^:export init []
  (on-js-reload)
  ;; Init app state just once.
  (events/fire! {:type :lemonade.events/init}))
