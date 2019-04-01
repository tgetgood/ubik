(ns ubik.ws
  "Mostly pinched (and trimmed down) from Chord
  (https://github.com/jarohen/chord/)."
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as p]
            [cognitect.transit :as transit]
            [org.httpkit.server :as http])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [falloleen.lang
            CoordinateFrame
            AffineWrapper
            Reflection
            Scaling
            Rotation
            Line
            Bezier
            Arc
            Circle
            Style]))

(defn read-transit [s]
  (let [bais (ByteArrayInputStream. (.getBytes s))]
    (transit/read (transit/reader bais :json))))

(def fall-handlers
  (transit/record-write-handlers
   CoordinateFrame
   AffineWrapper
   Reflection
   Scaling
   Rotation
   Line
   Bezier
   Arc
   Circle
   Style))

(defn write-transit [m]
  (let [baos (ByteArrayOutputStream.)]
         (transit/write (transit/writer baos :json {:handlers fall-handlers}) m)
         (.toString baos)))

(defn- on-close [ws read-ch write-ch]
  (http/on-close ws (fn [_]
                      (async/close! read-ch)
                      (async/close! write-ch))))

(defn connect-read-ch! [ws ch]
  (http/on-receive ws #(async/put! ch {:message %})))

(defn connect-write-ch! [ws ch]
  (async/go-loop []
    (let [msg (async/<! ch)]
      (when msg
        (http/send! ws msg)
        (recur)))))

(defn bidi-ch [read-ch write-ch on-close]
  (reify
    p/ReadPort
    (take! [_ handler]
      (p/take! read-ch handler))

    p/WritePort
    (put! [_ msg handler]
      (p/put! write-ch msg handler))

    p/Channel
    (close! [_]
      (p/close! read-ch)
      (p/close! write-ch)
      (on-close))))

(defn core-async-ch [httpkit-ch {:keys [formatter]
                                 :or   {formatter {:read  read-transit
                                                   :write write-transit}}}]
  (let [read-ch  (async/chan 32 (map (:read formatter)))
        write-ch (async/chan 32 (map (:write formatter)))]

    (connect-read-ch! httpkit-ch read-ch)
    (connect-write-ch! httpkit-ch write-ch)

    (on-close httpkit-ch read-ch write-ch)

    (bidi-ch read-ch write-ch #(when (http/open? httpkit-ch)
                                 (http/close httpkit-ch)))))

(defmacro with-channel
  "Extracts the websocket from the request and binds it to 'ch-name' in the body
   Arguments:
    req         - (required) HTTP-kit request map
    ch-name     - (required) variable to bind the channel to in the body
   Usage:
    (require '[clojure.core.async :as a])
    (with-channel req the-ws
      (a/go-loop []
        (when-let [msg (a/<! the-ws)]
          (println msg)
          (recur))))
    (with-channel req the-ws
      {:read-ch (a/chan (a/sliding-buffer 10))
       :write-ch (a/chan (a/dropping-buffer 5))}
      (go-loop []
        (when-let [msg (<! the-ws)]
          (println msg)
          (recur))))"

  [req ch-name & [opts & body]]

  (let [opts? (and (or (map? opts)
                       (:opts (meta opts)))
                   (seq body))
        body (cond->> body
               (not opts?) (cons opts))
        opts (when opts? opts)]

    `(http/with-channel ~req httpkit-ch#
       (let [~ch-name (core-async-ch httpkit-ch# ~opts)]
         ~@body))))
