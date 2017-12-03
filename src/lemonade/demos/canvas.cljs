(ns lemonade.demos.canvas
  (:require [lemonade.coordinates :as coords]
            [lemonade.core :as core]
            [lemonade.events :as events]
            [lemonade.events.canvas :as dom-events]
            [lemonade.events.hlei :as hlei]
            [lemonade.examples.elections :as elections]
            [lemonade.examples.infinite :as infinite]
            [lemonade.renderers.canvas :as rc]
            [lemonade.system :as system]
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
  (atom {:election-data elections/election-data}))

(defn base
  "Main render fn."
  [{:keys [election-data] :as state}]
  elections/ring-example
  #_(elections/election election-data))

(defn interactive-hud [render]
  (fn [state]
    [(render state) (assoc core/circle
                           :radius 200
                           :centre (or (::last-click state) [0 0])
                           :style {:fill :yellow
                                   :opacity 0.4
                                   :stroke :none})]))

(core/deftemplate ::event-layer
  {:handlers {}
   :base     []}
  {:type             ::core/composite
   :style            {}
   ::events/handlers handlers
   :contents         base})

(def event-test-handlers
  {::events/left-click (fn [{:keys [location]}]
                         {:mutation [assoc ::last-click location]})})

(defn event-test-wrapper [handler]
  (fn [state]
    {:type ::event-layer
     :base (handler state)
     ::events/handlers event-test-handlers}))

(defn canvas-width [elem]
  (fn []
    [(.-width elem) (.-height elem)]))

;; ideal scenario
(def handler
  (let [elem (canvas-elem)]
    (-> base
        ;; (infinite/example (canvas-width elem))
        event-test-wrapper
        window/wrap-windowing
        ;; interactive-hud
        hlei/wrap
        (coords/wrap-invert-coordinates elem))))

(defn on-js-reload []
  (fullscreen-canvas!)

  (let [elem (canvas-elem)]
    (system/initialise!
     {:event-system (dom-events/event-system elem)
      :render       (partial rc/draw! elem)
      :handler      handler
      :app-db       state})))

(defn ^:export init []
  (on-js-reload)
  ;; Init app state just once.
  (events/init-event-handlers!))
