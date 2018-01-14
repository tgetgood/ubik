(ns lemonade.demos.canvas
  (:require [goog.object :as obj]
            [lemonade.core :as core]
            [lemonade.events :as events]
            [lemonade.events.hlei :as hlei]
            [lemonade.hosts :as hosts]
            [lemonade.system :as system]
            [lemonade.window :as window]))

(enable-console-print!)

(defonce state
  (atom {}))

(defn base
  "Main render fn."
  [state]
  (assoc core/rectangle :width 100 :height 200))

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
      interactive-hud
      hlei/wrap))

(def host (hosts/html-canvas))

(defn on-js-reload []
  (system/fullscreen host)

  (system/initialise!
   {:host    host
    :handler handler
    :app-db  state}))

(defn ^:export init []
  (on-js-reload))
