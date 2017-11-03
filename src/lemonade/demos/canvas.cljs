(ns lemonade.demos.canvas
  (:require [lemonade.core :as core]
            [lemonade.draw :refer [start-event-loop]]
            [lemonade.examples.basic :as ex]
            [lemonade.geometry :as geometry]
            [lemonade.renderers.canvas :as rc]))

;; Setup

(enable-console-print!)

;;;;; Canvas Element handling

(defn canvas-elem []
  (.getElementById js/document "canvas"))

(defn canvas-container []
  (.getElementById js/document "canvas-container"))

(defn canvas-container-dimensions []
  (let [cc (canvas-container)]
    [(.-clientWidth cc) (.-clientHeight cc)]))

(defn set-canvas-size! [canvas [width height]]
  (set! (.-width canvas) width)
  (set! (.-height canvas) height))

(defn canvas-container-offset []
  (let [c (canvas-container)]
    [(.-offsetLeft c) (.-offsetTop c)]))

(defn fullscreen-canvas! []
  (let [[w h :as dim] (canvas-container-dimensions)]
    (set-canvas-size! (canvas-elem) dim)))

(defn context []
  (.getContext (canvas-elem) "2d"))

;;;;; Drawing

(defn clear-screen! []
  (let [[w h] (canvas-container-dimensions)]
    (.clearRect (context) 0 0 w h)))

(defn get-coord-inversion []
  (let [[_ h] (canvas-container-dimensions)]
    (geometry/atx [1 0 0 -1] [0 h])))

(def main
  (core/transform ex/ex (get-coord-inversion)))

;;;;; Export

(def stop (atom nil))

(defn ^:export init []
  (fullscreen-canvas!)

  (when @stop
    (@stop))

  (reset! stop
          (start-event-loop #'main clear-screen! rc/renderer (context))))

(defn on-js-reload []
  (init))
