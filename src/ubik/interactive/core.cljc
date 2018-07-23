(ns ubik.interactive.core
  (:require [clojure.core.async :as async :include-macros true]
            [clojure.walk :as walk]
            [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.core :as core]
            [ubik.geometry :as geo]
            [ubik.interactive.db :as db]
            [ubik.interactive.events :as events]
            [ubik.hosts :as hosts]
            [ubik.interactive.subs :as subs :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Event Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord RHandler [in xform])
(defrecord Handler [in out xform name])

(defn error [m]
  (throw (#?(:clj Exception. :cljs js/Error) m)))

(defn keyword-or-set [kos]
  (cond
    (keyword? kos) #{kos}
    (set? kos) kos
    :else (error "Handlers can only watch single events or sets of events.")))

(defn temp-key [name]
  [::temp-state name])

(defn emit
  ([db]
   (fn [rf]
     (rf db)))
  ([db ev]
   (fn [rf]
     (rf db ev)))
  ([db ev & evs]
   (fn [rf]
     (reduce rf (rf db ev) evs))))

(defn transducer
  "Create a (non-interruptable) transducer from xform, which is an fn of 2
  arguments which can all `emit` to pass values down the chain."
  [xform]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc x]
       (let [step (xform acc x)]
         (if (fn? step)
           (step rf)
           step))))))

(defn handler
  {:style/indent [1]}
  ;; TODO: This should be a macro that looks at rf and decides whether or not it
  ;; is already a transducer. If I can hide this from the user then that should
  ;; substantially improve the interface.
  ([watch f emit]
   (Handler. (keyword-or-set watch) emit f "anon$"))
  ([watch f emit name]
   (Handler. (keyword-or-set watch) emit f name)))

(defn db-handler [watch rf]
   (RHandler. (keyword-or-set watch) (transducer rf)) )

(macros/deftime

(defmacro defhandler
  {:style/indent [3]}
  [name k args methods]
  (let [multi (gensym)]
    `(do
       (defmulti ~multi (fn ~args (:type ~(second args))))
       ~@(map (fn [[k# v#]] `(defmethod ~multi ~k# ~args ~v#))
              methods)
       (def ~name
         (Handler. ~(into #{} (keys methods)) ~k (transducer ~multi) ~name))))))

(defn organise-handlers [handlers]
  (reduce (fn [hg h]
            (reduce (fn [hg w]
                      (if (contains? hg w)
                        (update hg w conj h)
                        (assoc hg w [h])))
                    hg (:in h)))
          {} handlers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Event Queue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IEventQueue
  (enqueue [this v]))

(defrecord EventQueue [queue buf-count buf-size]
   IEventQueue
   (enqueue [this v]
     (swap! buf-count inc)
     (if (<= buf-size buf-count)
       (error "Too many backlogged events. Cannot recover. Aborting.")
       (async/put! queue v))
     this))

(defn set-if-nil [ms k v]
  (map (fn [m]
         (if (contains? m k)
           m
           (assoc m k v)))
       ms))

(defn shunt-rf
  ([db]
   (if (::shunted db)
     db
     {::shunted true ::db db}))
  ([db ev]
   (if (::shunted db)
     (update db ::events conj ev)
     {::shunted true ::db db ::events [ev]})))

(defn transduce-1 [xform db ev]
  (let [f (xform shunt-rf)]
    (f (f db ev))))

(defn run-queue [handlers db event]
  (let [relevant (get handlers (:type event))]
    (loop [db db
           evs []
           hs relevant]
      (if (seq hs)
        (let [h (first hs)
              res (transduce-1 (:xform h) db event)
              evs (into evs (map #(assoc % :type (:out h)) (::events res)))]
          (recur (::db res) evs (rest hs)))
        [db evs]))))

(defn handle-effects [effects event]
  (run! #(% event) (get effects (:type event))))

(defn create-queue [handlers effects]
  (let [chan (async/chan 1000)
        queue (EventQueue. chan (atom 0) 1000)]
    (async/go-loop []
      (when-let [ev (async/<! chan)]
        (try
          (swap! (:buf-count queue) dec)
          (let [db (db/get-current-value)
                [next-db evs] (run-queue handlers db ev)]
            (when (seq evs)
              (reduce enqueue queue evs))
            (db/reset-db! next-db))
          (handle-effects effects ev)
          (catch #?(:clj Exception :cljs js/Error) e
            (println "We have a problem: " e)))
        (recur)))
    queue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Subscriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Distinction: subscriptions are reactive, signals are active. This is more
;; important than it may seem.

(def db db/db-sig)

(macros/deftime
  (defmacro defsub
  "Creates a subscription for form and binds it to a var with name. Sets the
  docstring approriately if provided."
  {:style/indent [1]}
  [name form]
    `(subs/defsub ~name ~form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Game Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ^:private continue? (atom nil))

(defn draw-loop
  "Starts an event loop which calls draw-fn on (app-fn @state-ref) each
  animation frame if @state-ref has changed."
  [world host check-sym]
  (let [recurrent (fn recurrent [counter last-run]
                    #?(:clj
                       ;; Need some kind of abstraction around animation frames.
                       ;; We can't be drawing in a busy loop like this
                       (core/draw! @world host)
                       :cljs
                       (js/window.requestAnimationFrame
                        (fn [now]
                          (when (= check-sym @continue?)
                            (let [w @world]
                              (when-not (= @db/the-world w)
                                (core/draw! w host)
                                (reset! db/the-world w)))
                            (recurrent (inc counter) now))))))]
    (recurrent 0 0)))


(defn listify-keys [m]
  (into {}
        (map (fn [[k v]] [k (if (sequential? v) v (list v))]) m)))

;; REVIEW: I've made this dynamic so that it can be swapped out by code
;; introspection programs which need to evaluate code and grab their handlers,
;; state atoms, etc.
;;
;; There's got to be a better way to get the desired dynamism
(defn ^:dynamic initialise!
  "Initialises the system, whatever that means right now."
  [{:keys [root host subs handlers init-db effects plugins]}]
  (db/set-once! init-db)
  (let [host (or host (hosts/default-host {}))
        handlers (into (mapcat :handlers plugins) handlers)
        plug-effects (map (fn [x] (listify-keys (:effects x))) plugins)
        _  (println plug-effects)
        effects (apply merge-with concat (listify-keys effects) plug-effects)
        hs (organise-handlers handlers)
        queue (create-queue hs effects)]
    (events/wire-events host #(enqueue queue %))

    (draw-loop root host (reset! continue? (gensym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aggregated API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def undo-plugin db/undo-plugin)
