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
;;;;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn error [m]
  (throw (#?(:clj Exception. :cljs js/Error) m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Event Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Multiplexer
  (inputs [this])
  (method [this input])
  (add-method [this key method]))

(defprotocol EmissionTracking
  (^:private set-emission! [this val]))

(defprotocol Stateful
  (get-state [this])
  (^:private set-state! [this state]))

(def ^:dynamic emit)

(deftype StatefulProcess
    #?(:clj [methods ^:volatile-mutable last-emission ^:volatile-mutable state]
       :cljs [methods ^:mutable last-emission ^:mutable state])
    ;; Deref
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    last-emission)

  Stateful
  (get-state [this]
    state)
  (set-state! [this s']
    (set! state s'))

  EmissionTracking
  (set-emission! [_ v]
    (set! last-emission v))

  Multiplexer
  (method [this input]
    (when-let [method (get methods input)]
      (let [emitter (fn ([s]
                         (fn [rf acc]
                           (set-state! this s)
                           (rf acc)))
                      ([s ev]
                       (fn [rf acc]
                         (set-state! this s)
                         (set-emission! this ev)
                         (rf acc ev)))
                      ([s ev & evs]
                       (fn [rf acc]
                         (set-state! this s)
                         (set-emission! this (last evs))
                         (reduce rf (rf acc ev) evs))))
            trans   (fn [xform]
                      (fn [rf]
                        (fn
                          ([] (rf))
                          ([acc] (rf acc))
                          ([acc x]
                           (let [step (binding [emit emitter] (xform x))]
                             (if (fn? step)
                               (step rf acc)
                               (do
                                 (set-state! this step)
                                 step)))))))]
        (trans
         (fn [e] (method state e))))))

  (add-method [_ k method]
    (StatefulTransducer. (assoc methods k method) ::uninitialised nil))

  (inputs [_]
    (into #{} (keys methods))))


(deftype StatelessProcess
    #?(:clj [methods ^:volatile-mutable last-emission]
       :cljs [methods ^:mutable last-emission])

  ;; Deref
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    last-emission)

  EmissionTracking
  (set-emission! [_ v]
    (set! last-emission v))

  Multiplexer
  (method [this input]
    (when-let [method (get methods input)]
      (let [emitter (fn ([s]
                         (fn [rf acc]
                           (rf acc)))
                      ([s ev]
                       (fn [rf acc]
                         (set-emission! this ev)
                         (rf acc ev)))
                      ([s ev & evs]
                       (fn [rf acc]
                         (set-emission! this (last evs))
                         (reduce rf (rf acc ev) evs))))
            trans   (fn [xform]
                      (fn [rf]
                        (fn
                          ([] (rf))
                          ([acc] (rf acc))
                          ([acc x]
                           (let [step (binding [emit emitter] (xform x))]
                             (if (fn? step)
                               (step rf acc)
                               step))))))]
        (trans method))))
  (inputs [_]
    (into #{} (keys methods)))

  (add-method [_ k method]
    (Transducer. (assoc methods k method) ::uninitialised)))

(defrecord TransducerProcess [methods]
  Multiplexer
  (inputs [_]
    (into #{} (keys methods)))
  (method [_ input]
    (get methods input))
  (add-method [_ input method]
    (TransducerProcess. (assoc methods input method))))

(defn watches? [process input]
  (contains? (inputs process) input))

(defn db-handler [watch rf]
   #_(RHandler. (keyword-or-set watch) (transducer rf)) )

;; Stateful processes need a uuid to key the storage.

(macros/deftime

  (defmacro process [state? multiplex]
    (if state?
      `(StatefulProcess. ~multiplex ::uninitialised nil)
      `(StatelessProcess. ~multiplex ::uninitialised)))

  (defmacro handler
    [listen tx]
    `(TransducerProcess. {~listen ~tx}))

  (defmacro defprocess
    {:style/indent [1]}
    [n bindings methods]
    `(def ~n
       (process ~(= 2 (count bindings))
                ~(into {} (map (fn [[k v]]
                                 [k `(fn ~bindings ~v)])
                               methods))))))

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
  ;; FIXME: Copied over from subs.cljc. Should use import-var from Tellman's
  ;; whatchacallit.
  (defmacro subscription
    "Given a form which derefs other subscriptions returns a new subscription
  that reacts to its dependencies. If form does not depend on any subscriptions
  then it is evaluated and it's (static) value returned."
    [form]
    `(subs/subscription ~form)))

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
        effects (apply merge-with concat (listify-keys effects) plug-effects)
        hs (organise-handlers handlers)
        queue (create-queue hs effects)]
    (events/wire-events host #(enqueue queue %))

    (draw-loop root host (reset! continue? (gensym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aggregated API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def undo-plugin db/undo-plugin)
