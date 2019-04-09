(ns ubik.server
  (:require [falloleen.core :as f]
            [clojure.core.async :as async]
            [org.httpkit.server :as server]
            [ubik.process :as process]
            [ubik.ws :as ws]))

(defonce ^:private stop-server-cb
  (atom nil))

(defonce clients
  (atom {}))

(defn broadcast! [message]
  (run! #(ws/send! % message) (vals @clients)))

(defn render [shape]
  (broadcast! {:signal :ubik.topology/screen :message shape}))

(defn display-code [code]
  (broadcast! {:signal :ubik.topology/code :message code}))

(def code-edits
  (process/signal ::edits))

(def interactions
  (process/signal ::interactions))

(defn handler [req]
  (server/with-channel req new-ch
    (let [name (keyword (gensym))]
      (swap! clients assoc name new-ch)
      (async/reduce (fn [_ {:keys [signal message]}]
                      (cond
                        (= signal ::edits)
                        (process/send code-edits message)

                        (= signal ::interactions)
                        (process/send interactions message)))
                    nil
                    (ws/read-channel new-ch))
      ;; TODO: listen to it...
      (server/on-close new-ch (fn [_] (swap! clients dissoc name))))))

(defn stop-server! []
  (when @stop-server-cb
    (run! #(server/close %) (vals @clients))
    (@stop-server-cb)
    (reset! clients {})))

(defn start-server! []
  (stop-server!)
  (let [s (server/run-server #'handler {:port 8080})]
    (reset! stop-server-cb s)))
