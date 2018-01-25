(ns lemonade.system
  (:require [lemonade.coordinates :as coords]
            [lemonade.events.hlei :as hlei]
            [lemonade.hosts :as hosts]
            [lemonade.hosts.protocol :as hp]
            [lemonade.state :as state]
            [lemonade.window :as window]))

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

;; REVIEW: I've made this dynamic so that it can be swapped out by code
;; introspection programs which need to evaluate code and grab their handlers,
;; state atoms, etc.
;;
;; There's got to be a better way to get the desired dynamism
(defn ^:dynamic initialise!
  "Initialises the system, whatever that means right now."
  [{:keys [app-db render host behaviour size]
    :or {app-db (atom {})
         host hosts/default-host
         size :fullscreen
         behaviour (comp hlei/wrap window/wrap-windowing)}}]

  (reset! state/internal-db app-db)

  (when (= size :fullscreen)
    (hp/fullscreen host)
    (hp/on-resize host (fn []
                      (hp/fullscreen host)
                      (swap! @state/internal-db update :lemonade.core/window assoc
                             :height (hp/height host)
                             :width (hp/width host)))))

  (when (and (vector? size) (= 2 (count size)))
    (apply hp/resize-frame host size))

  (when-not (:lemonade.core/window @@state/internal-db)
    (swap! @state/internal-db assoc :lemonade.core/window window/initial-window))

  (swap! @state/internal-db update :lemonade.core/window assoc
         :height (hp/height host)
         :width  (hp/width host))

  (let [{:keys [teardown setup]} (hp/event-system host)]
    (when teardown
      (teardown))

    (when setup
      (setup)))

  (let [handler (if (fn? render) render (constantly render))
        wrapped-handler (coords/wrap-invert-coordinates (behaviour handler))]
    (draw-loop @state/internal-db wrapped-handler (hp/render-fn host))))

(defn stop! []
  (when-let [sfn @idem]
    (sfn)))
