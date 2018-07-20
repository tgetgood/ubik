(ns ubik.interactive.impl)

(defprotocol Subscription
  (deps [_])
  (debug [_]))
