(ns lemonade.hosts.browser-canvas
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
  ;; TODO: This makes an assumption on the markup that is going to break
  ;; often. Complex, undocumented behaviour. Not good.
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

(defn host [element]
  (reify
    core/Host

    (event-system [_] (events/event-system element))
    (render-fn [_] (partial canvas-renderer/draw! element))
    (width [_] (obj/get element "width"))
    (height [_] (obj/get element "height"))
    (resize-frame [_ [width height]] (set-canvas-size! element [width height]))
    (on-resize [_ cb] (watch-resize cb) )
    (fullscreen [_] (fullscreen! element))))
