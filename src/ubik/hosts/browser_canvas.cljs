(ns ubik.hosts.browser-canvas
  "Hosts Ubik in an HTML Canvas element in a browser. Assumes a markup
  structure as follows:

  <div id='canvas-container'>
    <canvas id='canvas'></canvas>
  </div>"
  (:require [goog.object :as obj]
            [ubik.renderers.canvas :as canvas-renderer]
            [ubik.interactive.events.browser :as events]))

(defn canvas-elem []
  (js/document.getElementById "canvas"))

(defn canvas-container []
  (js/document.getElementById "canvas-container"))

(defn canvas-container-dimensions []
  (let [cc (canvas-container)]
    [(obj/get cc "clientWidth") (obj/get cc "clientHeight")]))

(defn set-canvas-size! [canvas [width height]]
  (obj/set canvas "width" width)
  (obj/set canvas "height" height))


(defn fullscreen! [elem]
  (let [[w h :as dim] (canvas-container-dimensions)]
    (set-canvas-size! elem dim)))

(defn watch-resize [cb]
  (let [running (atom false)]
    (set! (.-onresize js/window)
          (fn []
            (when (compare-and-set! running false true)
              (js/setTimeout
               (fn []
                 (when (compare-and-set! running true false)
                   (cb)))
               200))))))


(defn host [{:keys [size] :or {size :fullscreen}}]
  (let [elem (canvas-elem)]
    (cond
      (= :fullscreen size)                    (fullscreen! elem)
      (and (vector? size) (= 2 (count size))) (set-canvas-size! elem size)
      ;; TODO: Log bad size arg
      :else                                   nil)
    {:width     (fn [] (obj/get elem "width"))
     :height    (fn [] (obj/get elem "height"))
     :events    (events/event-signal elem)
     :render-fn (partial canvas-renderer/draw! elem)}))
