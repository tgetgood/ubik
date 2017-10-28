(ns lemonade.demos.canvas
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.gen.alpha :as gen]
            [lemonade.core :as core]
            [lemonade.demos.util :as util]
            [lemonade.examples.basic :as ex]
            [lemonade.geometry :as geometry]
            [lemonade.renderers.canvas :as rc]))

(enable-console-print!)

(defn context []
  (.getContext (util/canvas-elem) "2d"))

(defn get-coord-inversion []
  (let [[_ h] (util/canvas-container-dimensions)]
    (geometry/atx [1 0 0 -1] [0 h])))

(defn clear-screen! []
  (let [[w h] (util/canvas-container-dimensions)]
    (.clearRect (context) 0 0 w h)))

(defn draw! [shape]
  (let [shape* (core/transform shape (get-coord-inversion))
        render (rc/renderer shape*)]
    (clear-screen!)
    (render (context))))

(defn draw-rand []
  (let [shape (gen/generate (s/gen ::core/primitive-shape))]
    (s/explain ::core/shape shape)
    (draw! shape)))

(defn ^:export init []
  (util/fullscreen-canvas!)
  (draw! ex/ex))

(defn on-js-reload []
  (init))
