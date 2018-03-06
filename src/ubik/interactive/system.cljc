(ns ubik.interactive.system
  (:require [ubik.core :as core]
            clojure.walk
            [ubik.hosts :as hosts]
            [ubik.interactive.db :as db]
            [ubik.interactive.events :as events]
            [ubik.interactive.core :as spray]))

(defonce ^:private idem (atom nil))

(defonce ^:dynamic *profile* false)

(defn draw-loop
  "Starts an event loop which calls draw-fn on (app-fn @state-ref) each
  animation frame if @state-ref has changed."
  [world host sg]
  (when-let [stop @idem]
    (stop))
  (let [last-state (atom (gensym "NO-MATCH"))
        continue?  (atom true)]
    (letfn [(recurrent [counter last-run]
              #?(:clj
                 ;; Need some kind of abstraction around animation frames.
                 ;; We can't be drawing in a busy loop like this
                 (core/draw! world)
                 :cljs
                 (js/window.requestAnimationFrame
                  (fn [now]
                    (when @continue?
                      (let [the-world (spray/realise-world world sg)]
                        (when-not (= the-world @last-state)
                          (core/draw! the-world host)
                          (reset! last-state the-world)))
                      (if (and *profile* (< 1000 (- now last-run)))
                        (do
                          (println (* 1000 (/ counter (- now last-run))))
                          (recurrent 0 now))
                        (recurrent (inc counter) last-run)))))))]
      (recurrent 0 0)

      (reset! idem
              (fn []
                (reset! continue? false))))))

(defn with-defaults [opts]
  (merge
   {:app-db         (atom {})
    :size           :fullscreen
    :event-handlers {}}
   opts))

;; REVIEW: I've made this dynamic so that it can be swapped out by code
;; introspection programs which need to evaluate code and grab their handlers,
;; state atoms, etc.
;;
;; There's got to be a better way to get the desired dynamism
(defn ^:dynamic initialise!
  "Initialises the system, whatever that means right now."
  [opts]
  (let [{:keys [shape host subscriptions event-handlers effect-handlers]}
        (with-defaults opts)]

    ;; Register effect / coeffect handlers

    ;; Build event handlers

    ;; Initialise event system
    (reset! events/handlers {})
    (events/add-handlers event-handlers)

    ;; Preprocess render tree.

    (draw-loop shape (if host host core/*host*) subscriptions)))

(defn stop! []
  (when-let [sfn @idem]
    (sfn)))
