(ns ubik.interactive.core
  (:require [clojure.core.async :as async :include-macros true]
            [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.core :as core]
            [ubik.interactive.events :as events]
            [ubik.hosts :as hosts]
            [ubik.interactive.subs :as subs :include-macros true]
            [ubik.interactive.process :as process :include-macros true]
            [ubik.interactive.rt :as rt]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Game Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ^:private continue? (atom nil))

(defn draw-loop
  "Starts an event loop which calls draw-fn on (app-fn @state-ref) each
  animation frame if @state-ref has changed."
  [world host check-sym]
  (let [last-render (atom nil)
        recurrent (fn recurrent [counter last-run]
                    #?(:clj
                       ;; Need some kind of abstraction around animation frames.
                       ;; We can't be drawing in a busy loop like this
                       (core/draw! @world host)
                       :cljs
                       (js/window.requestAnimationFrame
                        (fn [now]
                          (when (= check-sym @continue?)
                            (let [w @world]
                              (when-not (= @last-render w)
                                (core/draw! w host)
                                (reset! last-render w)))
                            (recurrent (inc counter) now))))))]
    (recurrent 0 0)))


;; REVIEW: I've made this dynamic so that it can be swapped out by code
;; introspection programs which need to evaluate code and grab their handlers,
;; state atoms, etc.
;;
;; There's got to be a better way to get the desired dynamism
(defn ^:dynamic initialise!
  "Initialises the system, whatever that means right now."
  [{:keys [render-root edge host] :or
    {host (hosts/default-host {})
     edge {:sinks {} :sources {} :event-system events/default-event-system}}}]
  (let [es (:event-system edge)
        runtime (rt/system-parameters render-root)
        channels (rt/initialise-processes (:event-pipes runtime))]
    (events/teardown es host)
    (events/setup es host (:event-sources runtime)
                  (fn [k v]
                    (let [chs (get channels k)
                          events [(assoc v ::render-tree @render-root)]]
                      (run! (fn [c] (async/put! c events)) chs))))

    (draw-loop render-root host (reset! continue? (gensym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aggregated API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Subscriptions

;; Distinction: subscriptions are reactive, signals are active. This is more
;; important than it may seem.

(macros/deftime
  ;; FIXME: Copied over from subs.cljc. Should use import-var from Tellman's
  ;; Potemkin. Need to port it to cljs first though.
  (defmacro subscription
    "Given a form which derefs other subscriptions returns a new subscription
  that reacts to its dependencies. If form does not depend on any subscriptions
  then it is evaluated and it's (static) value returned."
    [form]
    `(subs/subscription ~form))

;;; Processes

  (defmacro process [& args]
    `(process/process ~@args))

  (defmacro defprocess
    {:style/indent [1]}
    [& args]
    `(process/defprocess ~@args))

  (defmacro emit [& args]
    `(process/emit ~@args)))

(def stateful-process process/stateful-process)

(def subscription? subs/subscription?)
