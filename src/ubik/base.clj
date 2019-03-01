(ns ubik.base)

(defprotocol Inspectable
  (debug [this]
    "Returns a dump of internal state for debugging types."))

(defprotocol Listener
  "A listener is a process that listens for input to other processes (thus any
  process that is not an event source is a Listener)."
  (inputs [this]
    "Returns the set of processes and subscriptions that this object listens
    to."))

(defn watches?
  "Returns true iff process reacts to events emitted by input."
  [listener input]
  (when (satisfies? Listener listener)
    (contains? (inputs listener) input)))
