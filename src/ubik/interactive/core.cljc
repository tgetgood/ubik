(ns ubik.interactive.core
  (:require [clojure.walk :as walk]
            [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.core :as core]
            [ubik.geometry :as geo]
            [ubik.interactive.db :as db]
            [ubik.interactive.events :as events]
            [ubik.hosts :as hosts]))

(defn emit
  ([v]
   (fn [_ rf acc]
     (rf acc v)))
  ([v & args]
   (fn [_ rf acc]
     (let [v' (rf acc v)]
       (if (reduced? v')
         v'
         (reduce rf v' args))))))

(defn emit-state
  ([s]
   (fn [sv rf acc]
     (vreset! sv s)
     acc))
  ([s v]
   (fn [sv rf acc]
     (vreset! sv s)
     (rf acc v)))
  ([s v & args]
   (fn [sv rf acc]
     (vreset! sv s)
     (let [v' (rf acc v)]
       (if (reduced? v')
         v'
         (reduce rf v' args))))))

(defn transducer
  ([next-fn]
   (fn [rf]
     (fn
       ([] (rf))
       ([acc] (rf acc))
       ([acc x]
        (let [emission (next-fn x)]
          (if (fn? emission)
            (emission nil rf acc)
            acc))))))
  ([init-state next-fn]
   (transducer init-state next-fn (constantly nil)))
  ([init-state next-fn flush-fn]
   (fn [rf]
     (let [state (volatile! init-state)]
       (fn
         ([] (rf))
         ([acc]
          (let [emission (flush-fn @state)]
            (if (fn? emission)
              (rf (unreduced (emission state rf acc)))
              (rf acc))))
         ([acc x]
          (let [emission (next-fn @state x)]
            (if (fn? emission)
              (emission state rf acc)
              acc))))))))

(defn map* [f]
  (transducer (fn [x] (emit (f x)))))

(defn filter* [p]
  (transducer (fn [x] (when (p x) (emit x)))))

(defn partition-all* [n]
  (transducer
   []
   (fn [buffer next]
     (let [buffer' (conj buffer next)]
       (if (= (count buffer') n)
         (emit-state [] buffer')
         ;; Poor name choice. We're not emitting anything.
         (emit-state buffer'))))
   (fn [buffer]
     (emit buffer))))

(defn partition-all** [n]
  (transducer
   (java.util.ArrayList. n)
   (fn [buffer next]
     (.add buffer next)
     (when (= n (.size buffer))
              (let [v (vec (.toArray buffer))]
                (.clear buffer)
                (emit v))))
   (fn [buffer]
     (when-not (.isEmpty buffer)
       (let [v (vec (.toArray buffer))]
         ;;clear first!
         (.clear buffer)
         (emit v))))))

(defn mapcat* [f]
  (transducer (fn [x] (apply emit (f x)))))

(defonce the-world (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Subscriptions
;;
;; Every signal can be modelled so that the current value is the last emission
;; of a transducer or the last result of a reducer. Reducers give us classic
;; foldp, but transducers give us stream to stream mappings that don't suffer
;; from spacetime leaks.
;;
;; This seems like big deal. The advantages of the reduced store, without the
;; loss of concurrency or the necessity to store interim results in the central
;; store (they're kept inside stateful transducers.
;;
;; In addition each isolated stateful process is thread isolated. So we get
;; safe, automatic parallelism without spacetime leaks.
;;
;; Question: What would programming this way be like?
;;
;; TODO: I need to write an underlying collection for signal
;; transduction. Vectors and Seqs are no good since we'd need to take the last
;; value of eductions for each update. Channels would work, but semantically
;; signals are like agents: they always have a value and you should be able to
;; simply deref them to get it without sending them a message or subscribing to
;; them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macros/deftime
  (defmacro simple-tx
    "Syntactic sugar for declaring stateful, undying, uninterruptible
  transducers.

  Local state is managed by the wrapper so that client code seems pure. This
  will enable replayability down the line. Not now though."
    [init & body]
    (let [init (if body init {})
          body (if body (first body) init)]
      `(fn [xf#]
         (let [state# (volatile! ~init)
               bfn# ~body]
           (fn
             ([] (xf#))
             ([acc#] (xf# acc#))
             ([acc# value#]
              (let [out# (~body @state# value#)]
                (when-let [ns# (:state out#)]
                  (vreset! state# ns#))
                (if-let [emit# (:emit out#)]
                  (xf# acc# emit#)
                  (if-let [emit-n# (:emit-n out#)]
                    (reduce xf# acc# emit-n#)
                    acc#))))))))))

(defn signal [tx input]
  (let [out (atom nil)
        no-res (gensym)
        rx  (fn ([] no-res) ([_ x] x))
        tx-cont (fn [x] (tx nil rx))]
    (add-watch input (keyword no-res)
               (fn [_ _ _ new-val]
                 (let [new-sig (tx-cont new-val)]
                   (when-not (= new-sig no-res)
                     (reset! out new-sig)))))
    out))

(defprotocol Signal
  (-value [this signal-graph]))

;; Token protocol
;; FIXME: Terrible name
(defprotocol ISignal)

(extend-protocol Signal
  ;; I'm not sure I like this...
  #?(:clj Object :cljs default)
  (-value [this _] this)

  nil
  (-value [_ _] nil))

;; TODO: Eventually we'll want more aggressive caching.
(deftype MemoizedSubscription []
  ISignal)

(deftype SimpleSubscription [dependencies reaction
                             ;; FIXME: Not synchronised
                             ^:volatile-mutable _last-args
                             ^:volatile-mutable _last-val]
  ISignal
  Signal
  (-value [_ sg]
    (let [subs (map (fn [s] (if (keyword? s) (get sg s) s)) dependencies)
          args (map #(-value % sg) subs)]

      (if (= _last-args args)
        _last-val
        (let [next-val (apply reaction args)]
          (set! _last-args args)
          (set! _last-val next-val)
          next-val)))))

(defrecord RefSub [ref]
  ISignal
  Signal
  (-value [_ _] @ref))

(def db (RefSub. db/app-db))

(defn subscription? [sig]
  (satisfies? ISignal sig))

(defn subscription
  {:style/indent [1]}
  [deps reaction]
  (SimpleSubscription. deps reaction (gensym "NOMATCH") nil))

(defn deref-signal
  "Returns the current value of a signal"
  [sig graph]
  (let [graph (assoc graph :db db)]
    (cond
      (keyword? sig) (-value (get graph sig) graph)
      (subscription? sig)  (-value sig graph)
      ;; TODO: Error logging
      :else          nil)))

#?(:clj
   (defn intern-subscription [form table]
     (let [k (second form)
           tv @table]
       (if (contains? tv k)
         (get tv k)
         (let [sym (gensym)]
           (if (compare-and-set! table tv (assoc tv k sym))
             sym
             (recur form table)))))))

#?(:clj
   (defn sub-checker [sym]
     (fn [form]
       (and (list? form)
            (= (count form) 2)
            (symbol? (first form))
            (= (first form) sym)
            (keyword? (second form))))))

#?(:clj
   (defmacro sub-form
     {:style/indent [1]
      :doc          "Returns a subscribed version of form.

  This subscription is a function which given a signal graph returns a value.

  The signal graph is just a map from keys to subscriptions.

  A subscription does not need to be part of the signal graph it receives (but
  probably will be). Recursive calls will end in disaster."}
     [operator form]
     (let [symbol-table (atom {})

           magic-sym?   (sub-checker operator)

           body         (walk/prewalk
                         (fn [f]
                           (if (magic-sym? f)
                             (intern-subscription f symbol-table)
                             f))
                         form)

           sym-seq      (seq @symbol-table)]
       (if (empty? sym-seq)
         form
         `(subscription [~@(map key sym-seq)]
            (fn [~@(map val sym-seq)]
              ~body))))))

#?(:clj
   (defmacro defsubs [name operator sub-map]
     `(def ~name
        ~(into {}
               (map (fn [[k# v#]] [k# `(sub-form ~operator ~v#)])
                    sub-map)))))

(defn walk-subscriptions
  "Walks render tree recursively replacing all subscriptions by their
  instantaneous value."
  [shape sg]
  (walk/prewalk
   (fn [form]
     (if (subscription? form)
       (walk-subscriptions (-value form sg) sg)
       form))
   shape))

(def reload (atom true))

(let [last-db (atom (gensym "AhAhAh"))]
  (defn realise-world
    "Returns the passed in shape with all subscriptions replaced by their
  current values."
    [shape subs]
    (if (and (not @reload) (identical? @last-db @db/app-db))
      @the-world
      (do
        (reset! reload false)
        (reset! last-db @db/app-db)
        (walk-subscriptions shape (assoc subs :db db))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-effect-handlers
  {:swap! (fn [f]
            (swap! db/app-db f))
   :reset! (fn [value]
             (reset! db/app-db value))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- dispatcher
  "Returns an event dispatch fn."
  ;; TODO: Will eventually need to use a queue and not block the main thread too
  ;; long. I can probably just lift the queue out of reframe
  [event-map effect-map]
  (fn dispatch! [event]
    (when-let [ev-handlers (get event-map (:type event))]
      (doseq [evh (if (fn? ev-handlers) [ev-handlers] ev-handlers)]
        (let [outcome (evh event)]
          (doseq [[effect arg] outcome]
            (when (contains? effect-map effect)
              ((get effect-map effect) arg))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal Bookkeeping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-by-tag [tag]
  (->> @the-world
       geo/branch-seq*
       (filter (fn [b]
                 (some (fn [s]
                         (contains? (core/get-tags s) tag))
                       b)))
       first
       geo/recombinator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Roundup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ^:private continue? (atom nil))

(defn draw-loop
  "Starts an event loop which calls draw-fn on (app-fn @state-ref) each
  animation frame if @state-ref has changed."
  [world host sg check-sym]
  (letfn [(recurrent [counter last-run]
            #?(:clj
               ;; Need some kind of abstraction around animation frames.
               ;; We can't be drawing in a busy loop like this
               (core/draw! world host)
               :cljs
               (js/window.requestAnimationFrame
                (fn [now]
                  (when (= check-sym @continue?)
                    (let [world (realise-world world sg)]
                      (when-not (= @the-world world)
                        (core/draw! world host)
                        (reset! the-world world))
                      (recurrent (inc counter) last-run)))))))]
    (recurrent 0 0)))

;; REVIEW: I've made this dynamic so that it can be swapped out by code
;; introspection programs which need to evaluate code and grab their handlers,
;; state atoms, etc.
;;
;; There's got to be a better way to get the desired dynamism
(defn ^:dynamic initialise!
  "Initialises the system, whatever that means right now."
  [{:keys [root host subscriptions event-handlers effect-handlers]}]
  ;; Register effect / coeffect handlers

  ;; Build event handlers

  ;; Initialise event system

  (let [dispatch-fn (dispatcher event-handlers
                               (merge effect-handlers
                                      default-effect-handlers))]
    (events/start-event-system! dispatch-fn))

  ;; Preprocess render tree.
  (let [host (or host (hosts/default-host {}))]
    ;; HACK: When hot reloading, you need to draw even if nothing has changed
    ;; because the canvas gets cleared.
    (when-let [world @the-world]
      (core/draw! world host))

    (reset! reload true)
    (draw-loop root host subscriptions (reset! continue? (gensym)))))
