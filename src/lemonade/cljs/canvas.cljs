(ns lemonade.cljs.canvas
  (:require [cljs.pprint :refer [pprint]]
            [cljs.spec.alpha :as s]
            [cljs.spec.gen.alpha :as gen]
            [lemonade.core :as core]
            [lemonade.examples.basic :as ex]
            [lemonade.geometry :as geometry]
            [lemonade.renderers.canvas :as rc]
            [lemonade.spec-gen :as spec-gen]))

(enable-console-print!)

(defn canvas-elem []
  (.getElementById js/document "canvas"))

(defn context []
  (.getContext (canvas-elem) "2d"))

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
    (geometry/atx [1 0 0 -1] [0 h])))

(defn clear-screen! []
  (let [[w h] (canvas-container-dimensions)]
    (.clearRect (context) 0 0 w h)))

(defn draw! [shape]
  (let [shape* (core/transform shape (get-coord-inversion))
        render (rc/renderer shape*)]
    (clear-screen!)
    (render (context))))

(defn draw-rand []
  (let [shape (gen/generate (s/gen ::core/primitive-shape spec-gen/nice-reals))]
    (pprint shape)
    (draw! shape)))

(defn ^:export init []
  (fullscreen-canvas!)
  (draw! ex/ex))

(defn on-js-reload []
  (init))
