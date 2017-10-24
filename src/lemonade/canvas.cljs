(ns lemonade.canvas
  (:require [cljs.spec.alpha :as s]
            [lemonade.core :as core]
            [lemonade.renderers.canvas :as rc]
            [lemonade.spec :as re]))


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

(defn ^:export init []
  (fullscreen-canvas!)
  (println "loaded")
  (let [ctx (.getContext (canvas-elem) "2d")
        render (rc/render-fn (s/conform ::sl/shape core/ex))]
    (render ctx))
  )

(defn on-js-reload []
  (init))
