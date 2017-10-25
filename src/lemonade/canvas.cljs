(ns lemonade.canvas
  (:require [cljs.pprint :refer [pprint]]
            [cljs.spec.alpha :as s]
            [cljs.spec.gen.alpha :as gen]
            [lemonade.core :as core]
            [lemonade.renderers.canvas :as rc]
            [lemonade.spec :as ls]))

(enable-console-print!)

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

(defn get-coord-inversion []
  (let [[_ h] (canvas-container-dimensions)]
    (core/atx [1 0 0 -1] [0 h])))

(defn clear-screen! [ctx]
  (let [[w h] ()]
    (.clearRect ctx 0 0 w h)))

(defn draw! [shape]
  (let [ctx (.getContext (canvas-elem) "2d")
        shape* (core/transform shape (get-coord-inversion))
        render (rc/renderer shape*)]
    (clear-screen! ctx)
    (render ctx)))

(defn draw-rand []
  (let [shape (gen/generate (s/gen ::ls/primitive-shape ls/nice-reals))]
    (pprint shape)
    (draw! shape)))

(defn ^:export init []
  (fullscreen-canvas!)
  (draw! core/ex))

(defn on-js-reload []
  (init))
