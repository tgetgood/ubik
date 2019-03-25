(ns ubik.process
  (:refer-clojure :exclude [send])
  (:require [clojure.core.async :as async]
            [clojure.datafy :refer [datafy]]
            [clojure.pprint :refer [pprint]]
            clojure.reflect
            [taoensso.timbre :as log]
            [ubik.util :refer [vmap]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Attempt the second
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Signal
  (send [this message] "Force signal to emit message to all listeners")
  (disconnect [this key])
  (^{:style/indent [2]} listen [this key cb]
    "Adds a listener to this signal. cb will be called immediately with the most
    recent message (if any) and then asyncronously with each subsequent
    message.

    cb should be a function of one argument. If it is called with nil, this
    indicates that the signal has closed and the cb will never be invoked
    again.

    key must be a unique key which is used to remove a listener using the
    disconnect method."))

(defprotocol Multiplexer
  (call [this wire message]
    "Process message from wire. Returns a function that takes a reducing
    function and an accumulator.")
  (wire [this n sig]))

(defrecord BasicSignal [name last-message listeners]
  ;; Messages have to be sent exactly once, to all listeners, and in the order
  ;; they are emitted. New listeners can only be added between messages.
  Signal
  (send [this message]
    (locking this
      (log/log
       (if (and (sequential? name) (= (first name) :ubik.events/text-area))
                 :trace
                 :debug)
       (str "\n" (with-out-str (pprint {:event-type "BasicSignal/send"
                                        :name       name
                                        :message    message}))))
      (reset! last-message message)
      (run! (fn [[_ f]] (f message)) @listeners)))
  (disconnect [this key]
    (locking this
      (swap! listeners dissoc key)
      nil))
  (listen [this key cb]
    (locking this
      (when (contains? @listeners key)
        (log/warn key "is already a registered listener on" name ". Ignoring."))
      (swap! listeners assoc key cb)
      (when-let [lm @last-message]
        (cb lm)))))

(defn signal [name]
  (BasicSignal. name (atom nil) (atom {})))

(defrecord MProcess [name method-map output-signal input-queue previous]
  Signal
  (send [this message]
    (log/warn (str "You should not be manually injecting messages into the"
                   " middle of the graph. Consistency will suffer."))
    ;; REVIEW: I should probably just not implement this, but it feels like it
    ;; will be so handy for debugging...
    (send output-signal message))
  (listen [this key cb]
    (listen output-signal key cb))
  (disconnect [this key]
    (disconnect output-signal key))

  Multiplexer
  (call [this k message]
    ;; Manual one-step transduce
    (log/debug (str "Processing message:" "\n"
                    (with-out-str (pprint {:event-type "MProcess/call"
                                           :name       name
                                           :wire       k
                                           :multiplex  method-map
                                           :message    message}))))
    (((get method-map k) send) output-signal message))
  (wire [this k sig]
    (listen sig name
      (fn [message]
        (async/put! input-queue [k message])))))

(defn stateless-xform [method]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc m]
       (let [res (method m)]
         (if-let [m (:emit res)]
           (rf acc m)
           (if-let [ms (:emit-all res)]
             (reduce rf acc ms)
             acc)))))))

(defn stateful-xform [state method]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc m]
       (locking state
         (let [res (method @state m)]
           ;; I'm interpretting returning nil as abort, or pass.
           (when res
             (reset! state res))
           (if-let [m (:emit res)]
             (rf acc m)
             (if-let [ms (:emit-all res)]
               (reduce rf acc ms)
               acc))))))))

(defn- process* [name node]
  (let [out  (signal [name :out])
        prev (:state (meta node))
        q    (async/chan (async/sliding-buffer 32))
        p    (MProcess. name node out q prev)]
    ;; REVIEW: Do I want to somehow put this go machine inside the object?
    (async/go-loop []
      (when-let [[meth msg] (async/<! q)]
        (try
          (call p meth msg)
          (catch Exception e
            (log/error "Exception in process go machine" name ": \n"
                       (with-out-str (pprint
                                      {:wire      meth
                                       :node      name
                                       :multiplex node
                                       :message   msg
                                       :exception (datafy e)})))))
        (recur)))
    p))

(defn process
  [name node]
  (if (fn? node)
    (process* name {:in node})
    (process* name node)))


(defrecord Effector [name f input-queue]
  Multiplexer
  (call [this k message]
    (log/debug (str "Effect:" "\n"
                    (with-out-str (pprint {:event-type "Effector/call"
                                           :wire       k
                                           :name       name
                                           :message    message}))))
    (f this message))
  (wire [this k sig]
    (listen sig name
      (fn [m]
        (async/put! input-queue [k m])))))

(defn effector [name f]
  (let [in  (async/chan (async/sliding-buffer 32))
        ;; A side effector, in this system, is a reducing function that ignores
        ;; the accumulation.
        f'  (fn [acc x]
              ;; These effects need to be executed serially.
              ;; TODO: Is this the best way to enforce that?
              (locking f
                (f x)))
        eff (Effector. name f' in)]
    (async/go-loop []
      (when-let [[k msg] (async/<! in)]
        (try
          (call eff k msg)
          (catch Exception e
            (log/error "Exception in effector go machine" name ": \n"
                       (with-out-str (pprint
                                      {:wire      k
                                       :name      name
                                       :message   msg
                                       :exception (datafy e)})))))
        (recur)))
    eff))



(defn max-args
  "Returns the maximal non-var-args arity of the function f."
  [f]
  (transduce (comp (filter #(= (:name %) 'invoke))
                   (map :parameter-types)
                   (map count))
             max
             0
             (:members (clojure.reflect/reflect f))))

(defn make-stateful-node [mm]
  (let [state (atom nil)]
    (with-meta
      (vmap (partial stateful-xform state) mm)
      {:state state})))

(defn make-node [name method-map]
  (process
   name
   (let [c (into #{} (map max-args) (vals method-map))]
     (assert (= 1 (count c)) "All methods of a node must have the same arity.")
     (cond
       (= #{1} c) (vmap stateless-xform method-map)
       (= #{2} c) (make-stateful-node method-map)
       :else      (throw (Exception. "Unknown node method type."))))))