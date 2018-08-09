(ns ubik.hosts.browser-canvas
  "Hosts Ubik in an HTML Canvas element in a browser. Assumes a markup
  structure as follows:

  <div id='canvas-container'>
    <canvas id='canvas'></canvas>
  </div>"
  (:require [goog.object :as obj]
            [ubik.renderers.canvas :as canvas-renderer]))

(defn canvas-elem [id]
  (js/document.getElementById  id))

(defn canvas-container [id]
  (let [container-id (str id "-container")]
    (js/document.getElementById container-id)))

(defn canvas-container-dimensions [id]
  (let [cc (canvas-container id)]
    [(obj/get cc "clientWidth") (obj/get cc "clientHeight")]))

(defn set-canvas-size! [canvas [width height]]
  (obj/set canvas "width" width)
  (obj/set canvas "height" height))

(defn fill-container! [host]
  (let [[w h :as dim] (canvas-container-dimensions (:id host))]
    (set-canvas-size! (:elem host) dim)))

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


(defn host [{:keys [size id] :or {size :fullscreen id "canvas"}}]
  (let [elem (canvas-elem id)
        this {:width     (fn [] (obj/get elem "width"))
              :height    (fn [] (obj/get elem "height"))
              :elem      elem
              :id        id
              :render-fn (partial canvas-renderer/draw! elem)}]
    (cond
      (= :fullscreen size)                    (fill-container! this)
      (and (vector? size) (= 2 (count size))) (set-canvas-size! elem size)
      ;; TODO: Log bad size arg
      :else                                   nil)
    this))
