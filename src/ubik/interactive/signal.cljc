(ns ubik.interactive.signal
  (:require #?(:cljs [cljs.core.async :as async :include-macros true]
               :clj [clojure.core.async :as async])))

(defn last-rf
  "Reducing fn that acts like last on a sequential collection"
  ([] nil)
  ([final] final)
  ([_ next] next))

(defn tapit [mult]
  (let [ch (async/chan)]
    (async/tap mult ch)
    ch))

(defprotocol ISignal
  (watch [this])
  (process-input [this input]))

(declare reduction-signal)

#?(:clj
   (deftype ReductiveSignal [^:volatile-mutable val ch mult update-fn]
     ISignal
     (watch [_]
       (tapit mult))
     (process-input [_ input]
       (let [v' (update-fn val input)]
         (set! val v')
         (async/put! ch v')))

     clojure.lang.IDeref
     (deref [_] val)

     clojure.lang.IReduce
     (reduce [this rf]
       (.reduce this rf val))

     clojure.lang.IReduceInit
     (reduce [this rf init]
       (reduction-signal init [this] rf)))

   :cljs
   (deftype ReductiveSignal [^:mutable val ch mult update-fn]
     ISignal
     (watch [_]
       (tapit mult))
     (process-input [_ input]
       (let [v' (update-fn val input)]
         (set! val v')
         (async/put! ch v')))

     IDeref
     (-deref [_] val)

     IReduce
     (-reduce [this rf]
       (-reduce this rf val))
     (-reduce [this rf initial]
       (reduction-signal initial [this] rf))))

(defn basic-signal
  "Returns a signal initialised to init. If no reducing function is supplied,
  defaults to reset! to last input semanatics."
  ([init]
   (basic-signal init last-rf))
  ([init rf]
   ;; Send backpressure all the way to the roots of the DAG. This will result in
   ;; event dropping if the app can't keep up. Is that reasonable? Is anything
   ;; reasonable at that point?
   (let [ch (async/chan 10)
        mult (async/mult ch)]
     (ReductiveSignal. init ch mult rf))))

(defn reduction-signal [init watches rf]
  (let [input (async/merge (map watch watches))
        sig (basic-signal init rf)]
    (async/go-loop []
      (when-let [i (async/<! input)]
        (process-input sig i)
        (recur)))
    sig))

(defn satom
  "Returns a signal that tracks the given atom.
  Great for testing, probably not for real use."
  [a]
  (let [sig (basic-signal @a)]
    (add-watch a ::signal (fn [_ _ _ n] (process-input sig n)))
    sig))

(defn combine [sigs]
  (when (seq sigs)
    (reduction-signal @(first sigs) sigs last-rf)))

(defn signal
  ([xform input]
   (transduce xform last-rf input))
  ([xform input & inputs]
   (transduce xform last-rf (combine (conj inputs input)))))