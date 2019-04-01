(ns ubik-front.core
  (:require [cljs.core.async :as async]
            [falloleen.core :as f]
            [falloleen.hosts :as hosts]
            [ubik-front.ws :as ws]))

(def host (hosts/default-host {:size :fullscreen}))

(def ws
  ;; Reset the ws every time we reload this ns.
  (let [a (atom nil)
        b (ws/ws-ch "ws://localhost:8080")]
    (async/go
      (reset! a (:ws-channel (async/<! b))))
    a))

(async/go-loop []
  (if @ws
    (when-let [im (async/<! @ws)]
      (println im)
      (f/draw! im host)
      (recur))
    (do
      (async/<! (async/timeout 100))
      (recur))))
