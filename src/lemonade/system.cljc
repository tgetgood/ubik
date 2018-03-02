(ns lemonade.system
  (:require [lemonade.core :as core]
            [lemonade.events.hlei :as hlei]
            clojure.walk

            [lemonade.hosts :as hosts]
            [lemonade.db :as db]
            [lemonade.window :as window]
            [lemonade.events :as events]
            [lemonade.spray :as spray]))

(defonce ^:private idem (atom nil))

(defonce ^:dynamic *profile* false)

(defn draw-loop
  "Starts an event loop which calls draw-fn on (app-fn @state-ref) each
  animation frame if @state-ref has changed."
  [world]
  (when-let [stop @idem]
    (stop))
  (let [last-state (atom (gensym "NO-MATCH"))
        continue?  (atom true)]
    (letfn [(recurrent [counter last-run]
              #?(:clj
                 (core/draw! (spray/reality world))
                 :cljs
                 (js/window.requestAnimationFrame
                  (fn [now]
                    (when @continue?
                      (let [the-world (spray/reality world)]
                        (when-not (= the-world @last-state)
                          (core/draw! the-world)
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
    :host           hosts/default-host
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
  (let [{:keys [app-db render host event-handlers size]}
        (with-defaults opts)]

    ;; Set screen size
    ;; Initialise internal DB
    ;; Set up event handling
    ;; Start draw loop

    (when core/*host*
      (core/teardown core/*host*))

    (core/setup host)
    (set! core/*host* host)

    (reset! db/app-db app-db)

    (reset! events/handlers {})
    (events/add-handlers event-handlers)

    (cond
      (= size :fullscreen)                    (core/fullscreen host)
      (and (vector? size) (= 2 (count size))) (core/resize-frame host size))

    (when-not (:lemonade.core/window @@db/app-db)
      (swap! @db/app-db assoc :lemonade.core/window window/initial-window))

    (swap! @db/app-db update :lemonade.core/window assoc
           :height (core/height host)
           :width  (core/width host))

    (let [world (spray/halucination render)]
      (draw-loop world))))

(defn stop! []
  (when-let [sfn @idem]
    (sfn)))
