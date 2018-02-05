(ns lemonade.renderers.canvas
  "HTML Canvas renderer."
  (:require [goog.object :as obj]
            [lemonade.core :as core]
            [lemonade.renderers.util :as util])
  (:require-macros [lemonade.renderers.canvas :refer [with-path-style]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti style-ctx (fn [ctx state [k v]] k))

(defn safe-style [ctx state style]
  (let [parent-style (:style state)
        child-style (merge {:stroke :black} style)]
    (doseq [[k v] child-style]
      (when (or (not (contains? parent-style k)) (= (get parent-style k) v))
        (style-ctx ctx state [k v])))))

(defn process-gradient [m])

(defn process-colour [c]
  (cond
    (= :none c)  "rgba(0,0,0,0)"
    (keyword? c) (name c)
    (string? c)  c
    (map? c)     (process-gradient c)
    :else        "rgba(0,0,0,0)"))

(defmethod style-ctx :stroke
  [ctx {:keys [zoom]} [_ v]]
  (let [c (process-colour v)]
    (when c
      (obj/set ctx "lineWidth" (/ 1 zoom))
      (obj/set ctx "strokeStyle" (if (fn? c) (c ctx) c)))))

(defmethod style-ctx :fill
  [ctx _ [_ v]]
  (let [c (process-colour v)]
    (when c
      (obj/set ctx "fillStyle" (if (fn? c) (c ctx) c)))))

(defmethod style-ctx :opacity
  [ctx _ [_ v]]
  (obj/set ctx "globalAlpha" v))

(defmethod style-ctx :font
  [ctx _ [_ v]]
  (obj/set ctx "font" v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti render-fn (fn [ctx state shape] (core/classify shape)))

(defn- render-all [ctx states shapes]
  (doseq [[state shape] (partition 2 (interleave states shapes))]
    (render-fn ctx state shape)))

(defn render
  "Returns a render function which when passed a context, renders the given
  shape."
  [ctx shape]
  (.save ctx)
  (.setTransform ctx 1 0 0 1 0 0)
  (render-fn ctx {:style {} :zoom 1 :in-path? false} shape)
  (.restore ctx)
  (.beginPath ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal render logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn :default
  [ctx state shape]
  (util/render-catchall shape))

(defmethod render-fn ::core/sequential
  [ctx state shape]
  (render-all ctx (repeat state) shape))

(defmethod render-fn ::core/template
  [ctx state shape]
  (render-fn ctx state (core/expand-template shape)))

(defmethod render-fn ::core/atx
  [ctx state {:keys [base-shape] {[a b c d] :matrix [e f] :translation} :atx}]
  (let [mag (util/magnitude a b c d)]
    (.save ctx)
    (.transform ctx a c b d e f)
    (render-fn ctx (update state :zoom * mag) base-shape)
    (.restore ctx)))

(defmethod render-fn ::core/path
  [ctx state {:keys [closed? contents style]}]
  (let [sub-state (-> state
                      (assoc :in-path? true)
                      (update :style #(merge style %)))
        ;; We need to make the first move explicitely despite the canvas docs
        ;; saying the first move is implicit...
        states (cons (assoc sub-state :override true)
                                 (repeat sub-state))]
    (.save ctx)
    (.beginPath ctx)

    (safe-style ctx state style)
    (render-all ctx states contents)

    (when closed?
      (.closePath ctx)
      (let [fill (-> sub-state :style :fill)]
        (when-not (or (nil? fill) (= :none fill))
          (.fill ctx))))

    (.stroke ctx)
    (.restore ctx)))

(defmethod render-fn ::core/composite
  [ctx state {:keys [style contents]}]
  (let [sub-state (update state :style #(merge style %))]
    (.save ctx)
    (safe-style ctx state style)
    (render-fn ctx sub-state contents)
    (.restore ctx)))

(defmethod render-fn ::core/frame
  [ctx state {:keys [contents width height] [x y] :corner}]
  (.save ctx)
  (.beginPath ctx)
  (.rect ctx x y width height)
  (.clip ctx )
  (render-fn ctx state contents)
  (.restore ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Leaf renderers
;;
;; At some point we have to render something, Less and less though, it would
;; appear.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn ::core/arc
  [ctx state {[x y] :centre r :radius :keys [from to style clockwise?] :as arc}]
  (with-path-style ctx
    (assoc state :override (:jump (meta arc))) style [(+ x r) y]
    (.arc ctx x y r from to (boolean clockwise?))))

(defmethod render-fn ::core/line
  [ctx state {:keys [from to style] :as o}]
  (with-path-style ctx state style from
    (.lineTo ctx (first to) (second to))))

(defmethod render-fn ::core/bezier
  [ctx state {[x1 y1] :from [x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2 style :style}]
  (with-path-style ctx state style [x1 y1]
    (.bezierCurveTo ctx cx1 cy1 cx2 cy2 x2 y2)))

(defmethod render-fn ::core/raw-text
  [ctx state {:keys [style corner text]}]
  (.save ctx)
  (safe-style ctx state style)
  (.fillText ctx text (first corner) (second corner))
  (.restore ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; API
;;
;; This should be the only outside reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- context [elem]
  (.getContext elem "2d"))

(defn clear-screen! [ctx w h]
  (.clearRect ctx 0 0 w h))

(defn draw! [canvas-element world]
  (doto (context canvas-element)
    (clear-screen! (.-width canvas-element) (.-height canvas-element))
    (render world)))
