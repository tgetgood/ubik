(ns ubik.interactive.process
  (:require [net.cgrand.macrovich :as macros :include-macros true]
            [ubik.interactive.subs :as subs]
            [ubik.interactive.base :as base]))

(defprotocol Multiplexer
  (method [this input])
  (add-method [this key method]))

(defprotocol EmissionTracking
  (^:private set-emission! [this val]))

(defprotocol Stateful
  (get-state [this])
  (^:private set-state! [this state]))

(def ^:dynamic emit)

(deftype StatefulProcess
    #?(:clj [method-map ^:volatile-mutable last-emission ^:volatile-mutable state]
       :cljs [method-map ^:mutable last-emission ^:mutable state])

  subs/Subscription

  base/Listener
  (inputs [_]
    (into #{} (keys method-map)))

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
    (when-let [method (get method-map input)]
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
    (StatefulProcess. (assoc method-map k method) ::uninitialised nil)))


(deftype StatelessProcess
    #?(:clj [method-map ^:volatile-mutable last-emission]
       :cljs [method-map ^:mutable last-emission])

  subs/Subscription

  base/Listener
  (inputs [_]
    (into #{} (keys method-map)))

  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    last-emission)

  EmissionTracking
  (set-emission! [_ v]
    (set! last-emission v))

  Multiplexer
  (method [this input]
    (when-let [method (get method-map input)]
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

  (add-method [_ k method]
    (StatelessProcess. (assoc method-map k method) ::uninitialised)))

(defrecord TransducerProcess [method-map]
  base/Listener
  (inputs [_]
    (into #{} (keys method-map)))

  Multiplexer
  (method [_ input]
    (get method-map input))
  (add-method [_ input method]
    (TransducerProcess. (assoc method-map input method))))



(defn db-handler [watch rf]
   #_(RHandler. (keyword-or-set watch) (transducer rf)) )

(defn stateful-process
  ([multiplexer] (stateful-process nil multiplexer))
  ([init-state multiplexer]
   (StatefulProcess. multiplexer init-state nil)))

(defn process [multiplexer]
  (StatelessProcess. multiplexer ::uninitialised))

(defn tprocess
  ([evmap]
   (TransducerProcess. evmap))
  ([listen xform]
   (tprocess {listen xform})))

(macros/deftime

  ;; TODO: Implement IMeta for process types and take metadata in
  ;; here. Especially docs. Though docs go on the var... I fell like metadata
  ;; will still be extremely useful for debugging. \s
  (defmacro defprocess
    {:style/indent [1]}
    [n bindings body-map]
    (let [method-map (into {} (map (fn [[k v]]
                                  `[~k (fn ~bindings ~v)])
                                body-map))]
      `(defonce ~n
         (~(if (= 2 (count bindings)) `stateful-process `process) ~method-map)))))
