(ns ubik.core
  (:require [falloleen.core :as f]
            [clojure.core.async :as async]
            [org.httpkit.server :as server]
            [ubik.process :as process]
            [ubik.ws :as ws]))

(defonce ^:private stop-server-cb
  (atom nil))

(defonce clients
  (atom #{}))

(defn render [shape]
  (run! #(async/put! % shape) @clients))

(defn handler [req]
  (ws/with-channel req new-ch
    (swap! clients conj new-ch)
    (async/put! new-ch
                {:signal :ubik.topology/screen
                 :message (f/with-style {:colour :blue}
                            (assoc f/circle :radius 200))})))

(defn stop-server! []
  (when @stop-server-cb
    (@stop-server-cb)))

(defn start-server! []
  (let [s (server/run-server #'handler {:port 8080})]
    (reset! stop-server-cb s)))
