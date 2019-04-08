(ns ubik.ws
  (:require [clojure.core.async :as async]
            [cognitect.transit :as transit]
            [org.httpkit.server :as http])
  (:import [falloleen.lang AffineWrapper Arc Bezier Circle CoordinateFrame Line Reflection Rotation Scaling Style]
           [java.io ByteArrayInputStream ByteArrayOutputStream]))

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

(defn send!
  "Sends message over ws. Current wire format is baked in as transit."
  [ws message]
  (http/send! ws (write-transit message)))

(defn read-channel
  "Returns a channel that will receive all messages from ws. Messages are
  assumed to be transit, which is read into clojure data automatically."
  [ws]
  (let [ch (async/chan 32 (map read-transit))]
    (http/on-receive ws #(async/put! ch %))
    ch))
