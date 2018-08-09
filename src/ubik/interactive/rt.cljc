(ns ubik.interactive.rt
  (:require [clojure.core.async :as async :include-macros true]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.interactive.base :as base]
            [ubik.core :as core]
            [ubik.interactive.db :as db]
            [ubik.interactive.events :as events]
            [ubik.hosts :as hosts]
            [ubik.interactive.subs :as subs :include-macros true]
            [ubik.interactive.process :as process]))

(defn merge-graphs [gs]
  (reduce (fn [acc g]
            (-> acc
                (update :all-procs set/union (:all-procs g))
                (update :push-map (partial merge-with set/union) (:push-map g))))
          {} gs))

(defn walk-signal-graph
  ([c]
   (walk-signal-graph #{} {} c))
  ([all push-map current]
   (if (keyword? current)
     {:push-map push-map :all-procs (conj all current)}
     (let [co     (if (var? current) @current current)
           inputs (base/inputs co)
           pm     (reduce (fn [push-map i]
                            (if (contains? push-map i)
                              (update push-map i conj current)
                              (assoc push-map i #{current})))
                          push-map inputs)]
       (merge-graphs (map #(walk-signal-graph (conj all current) pm %) inputs))))))

(defn external-events [w]
  (into #{} (filter keyword? (:all-procs (walk-signal-graph w)))))

(defn internal-events [w]
  (:push-map (walk-signal-graph w)))

(defn trace [s]
  (base/inputs s)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Event Queue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn error [m]
  (throw (#?(:clj Exception. :cljs js/Error) m)))

(defprotocol IEventQueue
  (enqueue [this ev]))

(defrecord EventQueue [chan buf-count buf-size]
   IEventQueue
   (enqueue [this ev]
     (swap! buf-count inc)
     (if (<= buf-size buf-count)
       (error "Too many backlogged events. Cannot recover. Aborting.")
       (async/put! chan ev))
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
     {::shunted true}))
  ([db ev]
   (if (::shunted db)
     (update db ::events conj ev)
     {::shunted true ::events [ev]})))

(defn transduce-1 [xform ev]
  (let [f (xform shunt-rf)]
    (f (f ev))))

(defn run-queue [handlers [etype event]]
  (let [relevant (get handlers etype)]
    (loop [evs []
           hs  relevant]
      (if (seq hs)
        (let [h       (first hs)
              handler (if (var? h) @h h)
              res     (transduce-1 (process/method handler etype) event)
              evs     (into evs (map (fn [ev] [handler ev]) (::events res)))]
          (recur  evs (rest hs)))
        evs))))

#_(defn handle-effects [effects event]
  (run! #(% event) (get effects (:type event))))

(defn create-queue []
  (let [chan (async/chan 1000)
        queue (EventQueue. chan (atom 0) 1000)]
    queue))

(defn start-queue-loop-process! [queue handlers]
  (async/go-loop []
    (when-let [ev (async/<! (:chan queue))]
      (try
        (swap! (:buf-count queue) dec)
        (let [evs (run-queue handlers ev)]
          (reduce enqueue queue evs))
        #_(handle-effects effects ev)
        (catch #?(:clj Exception :cljs js/Error) e
          (println "We have a problem: " e)))
      (recur))))
