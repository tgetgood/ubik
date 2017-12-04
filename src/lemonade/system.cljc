(ns lemonade.system)

(defonce ^:private idem (atom nil))

(defonce ^:dynamic *profile* false)

(defn draw-loop
  "Starts an event loop which calls draw-fn on (app-fn @state-ref) each
  animation frame if @state-ref has changed."
  ;; TODO: Deal with the possibility of static animations (animations that don't
  ;; depend on state).
  ;; IDEA: If hander returns a sequential with metadata ^:animation then treat
  ;; it as a sequence of frames and animate.
  [state-ref app-fn draw-fn]
  (when-let [stop @idem]
    (stop))
  (let [last-state (atom nil)
        continue?  (atom true)]
    (letfn [(recurrent [counter last-run]
              #?(:clj
                 (draw-fn (app-fn @state-ref))
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
                      (if (and *profile* (< 1000 (- now last-run)))
                        (do
                          (println (* 1000 (/ counter (- now last-run))))
                          (recurrent 0 now))
                        (recurrent (inc counter) last-run)))))))]
      (recurrent 0 0)

      (reset! idem
              (fn []
                (reset! continue? false))))))

;; Intentionally not defonce so that I can catch attempts to access it before
;; initalisation. The app-db passed into initialise! should be defonced, so hot
;; loading should be okay.
(def ^{:dynamic true :private true} *app-db* nil)

(defn handle-mutation
  "Like swap!, but signals global state change."
  [[f & args]]
  (apply swap! *app-db* f args))

(defn world
  "Returns the 'world', the root of the render tree. Note, that updates to the
  world tree are batched on animation frames, so this value is only up to date
  as of the last render."
  []
  (:lemonade.core/world @*app-db*))

(defn initialise!
  "Initialises the system, whatever that means right now."
  [{{:keys [event-system interceptor render-fn]} :host :keys [app-db handler]}]

  (set! *app-db* app-db)

  (when-let [f (:teardown event-system)]
    (f))

  (when-let [f (:setup event-system)]
    (f))

  (draw-loop app-db (interceptor handler) render-fn))
