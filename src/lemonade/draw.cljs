(ns lemonade.draw
  "Event Loops")

(defn start-event-loop
  "Starts an event loop which draws the contents of an iref to context with
  renderer. Runs on requestAnimationFrame.
  Returns a function which when invoked kills the event loop."
  [state-ref prerender render profile?]
  (let [last-state (atom nil)
        continue?  (atom true)]
    (letfn [(recurrent [counter last-run]
              (js/window.requestAnimationFrame
               (fn [now]
                 (let [state @state-ref]
                   (when-not (= state @last-state)
                     (let [world (prerender state)]
                       (swap! state-ref assoc :lemonade.core/world world)
                       (render world))
                     (reset! last-state state)))
                 (when @continue?
                   (if (and profile? (< 10000 (- now last-run)))
                     (do
                       (println (* 1000 (/ counter (- now last-run))))
                       (recurrent 0 now))
                     (recurrent (inc counter) last-run))))))]
      (recurrent 0 0)

      (fn []
        (reset! continue? false)))))
