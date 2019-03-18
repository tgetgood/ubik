(ns ubik.rt
  (:refer-clojure :exclude [send])
  (:require [clojure.core.async :as async :include-macros true]
            [clojure.datafy :refer [datafy]]
            clojure.reflect
            [clojure.set :as set]
            [clojure.walk :as walk]
            [taoensso.timbre :as log :include-macros true]
            [ubik.base :as base]
            [ubik.process :as process]
            [ubik.util :refer [vmap]]))

;; TODO: Get the static analysis code out of the runtime ns.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Signal Graph Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- source?
  "Returns true iff p is an event source.

  Currently that just means it's a keyword, but that will probably change."
  [p]
  (keyword? p))

(defn- process?
  "Returns true if x is a multiplexer process or a var that points to one."
  [x]
  (let [x (if (var? x) @x x)]
    (satisfies? process/Multiplexer x)))

(defn- sources
  "Returns the subset of the given forward signal graph where the only keys
  remaining are sources, that is processes which never receive input from the
  graph."
  [g]
  (let [valset (into #{} cat (vals g))]
    (apply disj (into #{} (keys g)) valset)))

(defn- follow-branch
  "Executes a breadth first search on graph and returns a seq of non-branching,
  non-cyclic paths."
  [graph start current seen]
  (if (contains? seen start)
    [current]
    (let [seen (conj seen start)
          current (conj current start)
          steps (get graph start)]
      (if (= 1 (count steps))
        (follow-branch graph (first steps) current seen)
        (into (if (< (count current) 2) [] [current])
              (mapcat #(follow-branch graph % [start] seen))
              steps)))))

(defn trace-source-signals
  "Returns a set of all signal pathways in g.
  g is assumed to be a map from graph nodes to sets of forward edges."
  [g]
  (let [source-signals (sources g)]
    (into #{} (mapcat #(follow-branch g % [] #{}) source-signals))))

(defn- devar
  "If x is a var deref it and return its value, otherwise return x."
  [x]
  (if (var? x)
    @x
    x))

(defn build-signal-graph
  "Given a process or subscription, walk back along inputs and return a map
  representing the communication graph of the network."
  ([current] (build-signal-graph current #{}))
  ([current seen]
   (when-not (or (contains? seen current) (source? current))
     (let [inputs (base/inputs (devar current))
           graph (into {} (map (fn [i] [i #{current}])) inputs)]
       (apply merge-with set/union graph
               (map #(build-signal-graph % (conj seen current)) inputs))))))

(defn- eager-subgraph
  "Returns the graph g with all nodes which are not process or event sources
  removed."
  [g]
  (into {} (comp (filter (fn [[k v]] (or (process? k) (source? k))))
                 (map (fn [[k v]] [k (into #{} (filter process?) v)])))
        g))

(defn build-pipe
  "Returns the current conventional datastructure for a process given a list of
  signal transduction steps in the process."
  [[x & xs]]
  {:in x :xform xs :out (last xs)})

(defn system-parameters
  "Analyses signal graph from root and returns the set of expected inputs to the
  resulting system as well as the set of processes that will need to be
  created and connected to initialise the system."
  [root]
  (let [graph (build-signal-graph root)
        inputs (sources graph)]
    {:event-sources inputs
     :event-pipes (->> graph
                       eager-subgraph
                       trace-source-signals
                       (map build-pipe))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Runtime logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-method-chain
  "Returns a vector of functions corresponding to the stages of process."
  [{:keys [in xform]}]
  (loop [s in
         [x & xs] xform
         ms []]
    (if x
      (let [dx (if (var? x) @x x)]
        (recur x xs (conj ms (process/method dx s))))
      ms)))

(defn- shunt-rf
  "Reducing function that discards the accumulated value and just collects
  further arguments.

  This is something of a kludge to implement the semantics of foldp, or if you
  prefer, scan over time."
  ([]
   [])
  ([db]
   db)
  ([db ev]
   (conj db ev)))

(defn go-machine
  "Creates a go-loop which reads messages off of input, transduces them
  according to process and distributes the resulting events (if any) to all
  listeners."
  [process input listeners]
  (let [xform (apply comp (get-method-chain process))]
    (async/go-loop []
      (when-let [events (async/<! input)]
        (try
          (when-let [events (transduce xform shunt-rf events)]
            (when (< 0 (count events))
              (log/debug
               "Transduced signal" (:in process)
               "to" (:out process)
               "\n"
               "Sending" (count events) "events to"
               (count listeners) "subscribed processes."))
            (when (seq events)
              (run! (fn [ch]
                      (async/put! ch events))
                    listeners)))
          (catch Exception e
            (log/error "Error in signal process " process ": ")))
        (recur)))))

(defn initialise-processes
  "Initialise a go machine for each process which connects the signal
  transduction with its inputs and outputs. Returns a map from processes to
  their input channels"
  [processes]
  (let [channels (map #(async/chan 100) processes)
        ch-map   (apply merge-with concat
                        (map (fn [k v] {k [v]})
                             (map :in processes)
                             channels))]
    (run! (fn [[p in-ch]]
            (go-machine p in-ch (get ch-map (:out p))))
          (partition 2 (interleave processes channels)))
    (log/info  "Created" (count processes) "processes listening to"
               (count ch-map) "events.")
    ch-map))

(defn kill-processes [ch-map]
  (run! async/close! (flatten (vals ch-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Attempt the second
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Signal
  (send [this message] "Force signal to emit message to all listeners")
  (^{:style/indent [1]} listen [this cb]
    "Adds a listener to this signal. cb will be called immediately with the most
    recent message (if any) and then asyncronously with each subsequent
    message.

    cb should be a function of one argument. If it is called with nil, this
    indicates that the signal has closed and the cb will never be invoked
    again."))

(defrecord BasicSignal [last-message listeners]
  ;; Messages have to be sent exactly once, to all listeners, and in the order
  ;; they are emitted. New listeners can only be added between messages.
  Signal
  (send [this message]
    (locking this
      (reset! last-message message)
      (run! #(% message) @listeners)))
  (listen [this cb]
    (locking this
      (swap! listeners conj cb)
      (when-let [lm @last-message]
        (cb lm)))))

(defn signal []
  (BasicSignal. (atom nil) (atom [])))

(defprotocol Multiplexer
  (call [this wire message]
    "Process message from wire. Returns a function that takes a reducing
    function and an accumulator.")
  (wire [this n sig]))

(defrecord MProcess [method-map output-signal input-queue previous]
  Multiplexer
  (call [this k message]
    ;; Manual one-step transduce
    (((get method-map k) send) output-signal message))
  (wire [this k sig]
    (listen sig
      (fn [message]
        (async/put! input-queue [k message])))))

(defn stateless-xform [method]
  (fn [xform]
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
               acc))))))))

(defn stateful-xform [state method]
  (fn [xform]
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
                 acc)))))))))

(defn process [method-map]
  (let [out (signal)
        prev (atom nil)
        q (async/chan (async/sliding-buffer 32))
        mm (vmap (prepare-xform prev) method-map)
        p (MProcess. mm out q prev (atom {}))]
    ;; REVIEW: Do I want to somehow put this go machine inside the object?
    (async/go-loop []
      (when-let [[meth msg] (async/<! q)]
        (try
          (call p meth msg)
          (catch Exception e
            (log/error (datafy e))))
        (recur)))
    p))
