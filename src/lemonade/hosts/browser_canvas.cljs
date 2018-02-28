(ns lemonade.hosts.browser-canvas
  "Hosts Lemonade in an HTML Canvas element in a browser. Assumes a markup
  structure as follows:

  <div id='canvas-container'>
    <canvas id='canvas'></canvas>
  </div>"
  (:require [goog.object :as obj]
            [lemonade.events.canvas :as events]
            [lemonade.renderers.canvas :as canvas-renderer]
            [lemonade.core :as core]))

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

(defn host [{:keys [resize always-fullscreen?]}]
  ;; TODO: Responsive with max-size and min-size, watch parent, aspect ratio? etc..
  (let [element (canvas-elem)
        this (reify
               core/Host
               (setup [_] (events/setup element lemonade.events/enqueue))
               (teardown [_] (events/teardown element))
               (render-fn [_] (partial canvas-renderer/draw! element))
               (width [_] (obj/get element "width"))
               (height [_] (obj/get element "height"))
               (resize-frame [_ [width height]]
                 (set-canvas-size! element [width height]))
               (fullscreen [_] (fullscreen! element)))]
    (cond
      resize (watch-resize resize)
      always-fullscreen? (watch-resize (fn [] (core/fullscreen this))))
    this))
