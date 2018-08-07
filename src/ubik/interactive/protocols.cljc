(ns ubik.interactive.protocols)


(defprotocol Subscription
  (inputs [_])
  (debug [_]))
