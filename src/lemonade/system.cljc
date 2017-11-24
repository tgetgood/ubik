(ns lemonade.system
  (:require [lemonade.events :as events]))

(let [idem (atom nil)]
  (defn draw-loop
    "Starts an event loop which calls draw-fn on (app-fn @state-ref) each
  animation frame if @state-ref has changed."
    ;; TODO: Deal with the possibility of static animations (animations that don't
    ;; depend on state).
    ;; IDEA: If hander returns a sequential with metadata ^:animation then treat
    ;; it as a sequence of frames and animate.
    [state-ref app-fn draw-fn profile?]
    (when-let [stop @idem]
      (stop))
    (let [last-state (atom nil)
          continue?  (atom true)]
      (letfn [(recurrent [counter last-run]
                #?(:clj
                   (draw-fn (app-fn @state-ref ))
                   :cljs
                   (js/window.requestAnimationFrame
                    (fn [now]
                      (let [state @state-ref]
                        (when-not (= state @last-state)
                          (let [world (app-fn state)]
                            (swap! state-ref assoc :lemonade.core/world world)
                            (draw-fn world))
                          (reset! last-state @state-ref)))
                      (when @continue?
                        (if (and profile? (< 10000 (- now last-run)))
                          (do
                            (println (* 1000 (/ counter (- now last-run))))
                            (recurrent 0 now))
                          (recurrent (inc counter) last-run)))))))]
        (recurrent 0 0)

        (reset! idem
                (fn []
                  (reset! continue? false)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Plugins (middleware?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initialise! [{:keys [render app-db handler profile? event-system]}]
  (when-let [f (:teardown event-system)]
    (f))

  (when-let [f (:setup event-system)]
    (f))

  (draw-loop app-db handler render profile?))
