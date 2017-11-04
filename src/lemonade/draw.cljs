(ns lemonade.draw
  "Event Loops")

(defn draw! [render-fn context]
  (if context
    (render-fn context)
    (render-fn)))

(defn start-event-loop
  "Starts an event loop which draws the contents of an iref to context with
  renderer. Runs on requestAnimationFrame.
  Returns a function which when invoked kills the event loop."
  [state-ref renderer & [context]]
  (let [last-state (atom nil)
        counter (atom 0)
        continue? (atom true)
        frame (fn [state]
                (when-not (= state @last-state)
                  (reset! last-state state)
                  (draw! (renderer state) context)))]
    (letfn [(recurrent []
              (js/window.requestAnimationFrame
               (fn []
                 (frame @state-ref)
                 (when @continue?
                   (js/window.requestAnimationFrame recurrent)))))]
      (recurrent)

      (fn []
        (reset! continue? false)))))
