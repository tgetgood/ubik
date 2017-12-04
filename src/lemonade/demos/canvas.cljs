(ns lemonade.demos.canvas
  (:require [goog.object :as obj]
            [lemonade.core :as core]
            [lemonade.events :as events]
            [lemonade.events.hlei :as hlei]
            [lemonade.examples.elections :as elections]
            [lemonade.hosts :as hosts]
            [lemonade.system :as system]
            [lemonade.window :as window]))

;; Setup

(enable-console-print!)

;;;;; Canvas Element handling

(defn canvas-elem []
  (js/document.getElementById "canvas"))

(defn canvas-container []
  (js/document.getElementById "canvas-container"))

(defn canvas-container-dimensions []
  (let [cc (canvas-container)]
    [(obj/get cc "clientWidth") (obj/get cc "clientHeight")]))

(defn set-canvas-size! [canvas [width height]]
  (obj/set canvas "width" width)
  (obj/set canvas "height" height))

(defn canvas-container-offset []
  (let [c (canvas-container)]
    [(obj/get c "offsetLeft") (obj/get c "offsetTop")]))

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

;; ideal scenario
(def handler
  (-> base
      event-test-wrapper
      window/wrap-windowing
      ;; interactive-hud
      hlei/wrap))

(defn on-js-reload []
  (fullscreen-canvas!)

  (system/initialise!
   {:host    (hosts/html-canvas (canvas-elem))
    :handler handler
    :app-db  state}))

(defn ^:export init []
  (on-js-reload))
