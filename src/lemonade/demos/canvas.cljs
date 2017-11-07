(ns lemonade.demos.canvas
  (:require [clojure.string :as string]
            [lemonade.core :as core]
            [lemonade.draw :refer [start-event-loop]]
            [lemonade.examples.elections :as elections]
            [lemonade.geometry :as geometry]
            [lemonade.renderers.canvas :as rc]
            [lemonade.space :as space]
            [lemonade.window :as window]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn c-space-point
  "Returns Inverted pixel coordinates (origin in the lower left) of the event
  e."
  [e]
  (let [[_ h] (canvas-container-dimensions)
        [ox oy] (canvas-container-offset)]
    [(- (.-clientX e) ox) (- h (- (.-clientY e) oy))]))

(defonce window (atom {:zoom 1 :offset [0 0]}))

(def main elections/election)

(def handlers
  (let [drag-state (atom nil)]
    {:mouse-down (fn [e]
                   (reset! drag-state (c-space-point e)))
     :mouse-up   (fn [e]
                   (reset! drag-state nil))

     :click      (fn [e]
                   (println
                    (space/trace main
                     (c-space-point e))))

     ;; :key-down
     ;; (fn [e]
     ;;             (cond
     ;;               (= (.-key e) "j") (db/next-slide)
     ;;               (= (.-key e) "k") (db/prev-slide)
     ;;               :else nil))

     :mouse-move (fn [e]
                   (when @drag-state
                     (let [q     (c-space-point e)
                           p     @drag-state
                           delta (mapv - p q)]
                       (reset! drag-state q)
                       (swap! window window/update-offset delta))))
     :wheel      (fn [e]
                   (let [p  (c-space-point e)
                         dz (window/normalise-zoom (js/parseInt (.-deltaY e)))]
                     (swap! window window/update-zoom p dz)))}))

(def canvas-event-handlers
  (into {}
        (map (fn [[k v]]
               [(keyword (str "on-" (name k))) v])
          handlers)))

;;;;; Handler Registration

(defn kw->js [kw]
  (string/replace (name kw) #"-" ""))

(defonce registered-listeners (atom nil))

(defn register-handlers! [elem]
  (reset! registered-listeners handlers)
  ;; HACK: Allows keypress events on canvas
  (aset elem "tabIndex" 1000)
  (doseq [[event cb] @registered-listeners]
    (.addEventListener elem (kw->js event) cb)))

(defn remove-handlers! [elem]
  (doseq [[event cb] @registered-listeners]
    (.removeEventListener elem (kw->js event) cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clear-screen! [ctx]
  (let [[w h] (canvas-container-dimensions)]
    (.clearRect ctx 0 0 w h)))

(defn render [frame]
  (doto (context)
    clear-screen!
    (rc/render frame)))

(defn get-coord-inversion []
  (let [[_ h] (canvas-container-dimensions)]
    (geometry/atx [1 0 0 -1] [0 h])))

(defn prerender [window]
  (-> main
      (core/transform (window/windowing-atx window))
      (core/transform (get-coord-inversion))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce stop (atom nil))

(defn ^:export init []
  (fullscreen-canvas!)

  (doto (canvas-elem)
    remove-handlers!
    register-handlers!)

  (when @stop
    (@stop))

  (reset! stop
          (start-event-loop window prerender render false)))

(defn on-js-reload []
  (init))
