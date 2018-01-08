(ns lemonade.system
  (:require [lemonade.coordinates :as coords]
            [lemonade.window :as window]))

(defprotocol Host
  (event-system [this])
  (render-fn [this])
  (width [this] "Current frame width")
  (height [this] "Current frame height")
  (resize-frame [this [width height]])
  (fullscreen [this]))

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
                    (when @continue?
                      (let [state @state-ref]
                        (when-not (= state @last-state)
                          (let [world (app-fn state)]
                            (swap! state-ref assoc :lemonade.core/world world)
                            (draw-fn world))
                          (reset! last-state @state-ref)))
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
(def ^:private internal-db (atom nil))

(defn handle-mutation
  "Like swap!, but signals global state change."
  [[f & args]]
  (apply swap! @internal-db f args))

(defn world
  "Returns the 'world', the root of the render tree. Note, that updates to the
  world tree are batched on animation frames, so this value is only up to date
  as of the last render."
  []
  (:lemonade.core/world @@internal-db))

;; REVIEW: I've made this dynamic so that it can be swapped out by code
;; introspection programs which need to evaluate code and grab their handlers,
;; state atoms, etc.
;;
;; There's got to be a better way to get the desired dynamism
(defn ^:dynamic initialise!
  "Initialises the system, whatever that means right now."
  [{:keys [app-db handler host]}]

  (reset! internal-db app-db)

  (when-not (:lemonade.core/window @@internal-db)
    (swap! @internal-db assoc :lemonade.core/window window/initial-window))

  (swap! @internal-db update :lemonade.core/window assoc
         :height (height host)
         :width  (width host))

  (let [{:keys [teardown setup]} (event-system host)]
    (when teardown
      (teardown))

    (when setup
      (setup)))

  (let [wrapped-handler (coords/wrap-invert-coordinates handler)]
    (draw-loop @internal-db wrapped-handler (render-fn host))))


(defn stop! []
  (when-let [sfn @idem]
    (sfn)))
