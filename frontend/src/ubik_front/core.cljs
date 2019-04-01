(ns ubik-front.core
  (:require [cljs.core.async :as async]
            [falloleen.core :as f]
            [falloleen.hosts :as hosts]
            [ubik-front.events :as events]
            [ubik-front.ws :as ws]))

(def host
  (hosts/default-host {:size :fullscreen}))

(def ws
  "The websocket connection to the server."
  (atom nil))

(def last-draw (atom nil))

(defn init! []
  (let [b (ws/ws-ch "ws://localhost:8080")]
    (async/go
      (let [ch (:ws-channel (async/<! b))]
        (reset! ws ch)
        (events/monitor host ch)
        (async/go-loop []
          (when-let [im (async/<! ch)]
            (reset! last-draw im)
            (f/draw! im host)
            (recur)))))))

(init!)
