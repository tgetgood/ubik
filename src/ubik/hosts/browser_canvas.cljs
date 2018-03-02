(ns ubik.hosts.browser-canvas
  "Hosts Ubik in an HTML Canvas element in a browser. Assumes a markup
  structure as follows:

  <div id='canvas-container'>
    <canvas id='canvas'></canvas>
  </div>"
  (:require [goog.object :as obj]
            [clojure.string :as string]
            [ubik.renderers.canvas :as canvas-renderer]
            [ubik.core :as core]))

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

(defn- kw->js
  "Returns js property name corresponding to idiomatic keyword."
  [kw]
  (string/replace (name kw) #"-" ""))

(defn- oget
  "Unsafe and fast version of goog.object/get."
  [o k]
  (unchecked-get o k))

(defn- pixel-point
  "Returns pixel clicked on relative to canvas element. Assumes origin in the
  bottom left."
  [elem e]
  [(- (oget e "clientX") (oget elem "offsetLeft"))
   (- (oget elem "height") (- (oget e "clientY") (oget elem "offsetTop")))])

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

(defn ^:private event-map
  "Returns a map of event handlers for elem."
  [elem]
  {:context-menu (fn [e]
                   (.preventDefault e)
                   (.stopPropagation e))

   ;; This is to make sure keyboard events happen at all.
   :mouse-over   (fn [e]
                   (.preventDefault e)
                   (.focus elem))

   :mouse-down   (fn [e]
                   (.preventDefault e)
                   (let [b (.-button e)
                         p (pixel-point elem e)]
                     (case b
                       ;; Only handling left click for now.
                       0 {:type     :ubik.events/left-mouse-down
                          :location p}
                       nil)))

   :touch-move   (fn [e]
                   (.preventDefault e))

   :mouse-move   (fn [e]
                   (.preventDefault e)
                   {:type     :ubik.events/mouse-move
                    :location (pixel-point elem e)})

   :mouse-up     (fn [e]
                   (.preventDefault e)
                   ;; REVIEW: Is this really to right place to decide what
                   ;; buttons a mouse has? We do need some kind of layer between
                   ;; "button 0" and "left click", but here might not be the
                   ;; place...
                   (let [b (.-button e)
                         p (pixel-point elem e)]
                     (case b
                       ;; Only handling left click for now.
                       0 {:type     :ubik.events/left-mouse-up
                          :location p}
                       nil)))

   :wheel        (fn [e]
                   (.preventDefault e)
                   (let [mag (if (= 1 (oget e "deltaMode")) 15 1)
                         dx  (* mag (js/parseInt (oget e "deltaX")))
                         dy  (* mag (js/parseInt (oget e "deltaY")))]
                     {:type     :ubik.events/wheel
                      :location (pixel-point elem e)
                      :dx       dx
                      :dy       dy}))

   :key-down     (fn [e]
                   (.preventDefault e)
                   {:type :ubik.events/key-down
                    :raw  e})

   :key-up       (fn [e]
                   (.preventDefault e)
                   {:type :ubik.events/key-up
                    :raw  e})})

(defonce ^:private registered-listeners (atom {}))

(defn teardown [elem]
  (doseq [[event cb] @registered-listeners]
    (.removeEventListener elem (kw->js event) cb)))

(defn setup [elem dispatch-fn]
  (let [handlers (event-map elem)]
    (reset! registered-listeners handlers)
    (doseq [[event cb] handlers]
      (.addEventListener elem (kw->js event) cb))))

(defn host [{:keys [resize always-fullscreen?]}]
  ;; TODO: Responsive with max-size and min-size, watch parent, aspect ratio? etc..
  (let [element (canvas-elem)
        this (reify
               core/Host
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
