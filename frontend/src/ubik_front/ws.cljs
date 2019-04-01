(ns ubik-front.ws
  (:require [cljs.core.async :as async]
            [cljs.core.async.impl.protocols :as p]
            [cognitect.transit :as transit]
            falloleen.lang)
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]
                   [ubik-front.macros :refer [record-read-handlers]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Transit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(println (record-read-handlers "Style"))

(def reader-map
  (record-read-handlers
   "Style"
   "AffineWrapper"
   "CoordinateFrame"
   "Reflection"
   "Scaling"
   "Rotation"
   "Line"
   "Bezier"
   "Arc"
   "Circle"))

(defn read-transit [s]
   (transit/read (transit/reader :json {:handlers reader-map}) s))

(defn write-transit [m]
   (transit/write (transit/writer :json) m))
(defn- create-ws [url]
  (js/WebSocket. url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Channels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Web Sockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn connect-read-ch! [ws ch]
  (set! (.-onmessage ws)
     (fn [ev]
       (let [message (.-data ev)]
         (async/put! ch message)))))


(defn connect-write-ch! [ws ch]
  (go-loop []
    (let [msg (<! ch)]
      (when msg
        (.send ws msg)
        (recur)))))

(defn close-event->maybe-error [ev]
  (when-not (.-wasClean ev)
    {:reason (.-reason ev)
     :code (.-code ev)}))

(defn ws-ch
  "Creates websockets connection and returns a 2-sided channel when the
  websocket is opened.
   Arguments:
    ws-url           - (required) link to websocket service

  See
  https://github.com/websockets/ws/blob/master/doc/ws.md#new-websocketaddress-protocols-options"
  [ws-url & [{:keys [formatter] :or {formatter {:read  read-transit
                                                :write write-transit}}}]]
  (let [web-socket (create-ws ws-url)
        read-ch    (async/chan 32 (map (:read formatter)))
        write-ch   (async/chan 32 (map (:write formatter)))

        open-ch  (async/chan)
        close-ch (async/chan)]

    (set! (.-binaryType web-socket) "arraybuffer")

    (connect-read-ch! web-socket read-ch)
    (connect-write-ch! web-socket write-ch)

    (set! (.-onopen web-socket) #(async/put! open-ch %))

    (set! (.-onclose web-socket) #(async/put! close-ch %))

    (let [ws-chan    (bidi-ch read-ch write-ch #(.close web-socket))
          initial-ch (async/chan)]

      (go-loop [opened? false]
        (alt!
          open-ch ([_]
                   (async/>! initial-ch {:ws-channel ws-chan})
                   (async/close! initial-ch)
                   (recur true))

          close-ch ([ev]
                    (let [maybe-error (close-event->maybe-error ev)]
                      (when maybe-error
                        (async/>! (if opened?
                                    read-ch
                                    initial-ch)
                                  {:error maybe-error}))

                      (async/close! ws-chan)
                      (async/close! initial-ch)))))

      initial-ch)))
