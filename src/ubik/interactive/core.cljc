(ns ubik.interactive.core
  (:require [clojure.walk :as walk]
            [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.core :as core]
            [ubik.geometry :as geo]
            [ubik.interactive.db :as db]
            [ubik.interactive.events :as events]
            [ubik.hosts :as hosts]))

(defonce ^:private continue? (atom nil))

(defn draw-loop
  "Starts an event loop which calls draw-fn on (app-fn @state-ref) each
  animation frame if @state-ref has changed."
  [world host sg check-sym]
  (let [the-world (atom nil)
        recurrent (fn recurrent [counter last-run]
                    #?(:clj
                       ;; Need some kind of abstraction around animation frames.
                       ;; We can't be drawing in a busy loop like this
                       (core/draw! world host)
                       :cljs
                       (js/window.requestAnimationFrame
                        (fn [now]
                          (when (= check-sym @continue?)
                            (when-not (= @the-world world)
                              (core/draw! world host)
                              (reset! the-world world))
                            (recurrent (inc counter) last-run))))))]
    (recurrent 0 0)))

;; REVIEW: I've made this dynamic so that it can be swapped out by code
;; introspection programs which need to evaluate code and grab their handlers,
;; state atoms, etc.
;;
;; There's got to be a better way to get the desired dynamism
(defn ^:dynamic initialise!
  "Initialises the system, whatever that means right now."
  [{:keys [root host subscriptions event-handlers effect-handlers]}]
  ;; Register effect / coeffect handlers

  ;; Build event handlers

  ;; TODO: Initialise event system

  ;; Preprocess render tree.
  (let [host (or host (hosts/default-host {}))]

    (draw-loop root host subscriptions (reset! continue? (gensym)))))
