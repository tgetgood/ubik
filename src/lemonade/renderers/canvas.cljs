(ns lemonade.renderers.canvas
  "HTML Canvas renderer."
  (:require [lemonade.core :as core]
            [lemonade.geometry :as geometry])
  (:require-macros [lemonade.renderers.canvas :refer [with-path-style]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private noop
  "What a render-fn returns if it wants to do nothing."
  (constantly nil))

(def default-style
  "Default style of images in lemonade."
  {:stroke  :black
   :fill    :none
   :opacity 1
   :font    "sans serif 10px"})

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
    :else        nil))

(defmethod style-ctx :stroke
  [ctx {:keys [zoom]} [_ v]]
  (let [c (process-colour v)]
    (when c
      (aset ctx "lineWidth" (/ 1 zoom))
      (aset ctx "strokeStyle" (if (fn? c) (c ctx) c)))))

(defmethod style-ctx :fill
  [ctx _ [_ v]]
  (let [c (process-colour v)]
    (when c
      (aset ctx "fillStyle" (if (fn? c) (c ctx) c)))))

(defmethod style-ctx :opacity
  [ctx _ [_ v]]
  (aset ctx "globalAlpha" v))

(defmethod style-ctx :font
  [ctx _ [_ v]]
  (aset ctx "font" v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti render-fn (fn [ctx state shape] (:type shape)))

(defn- render-all [ctx states shapes]
  (doseq [[state shape] (partition 2 (interleave states shapes))]
    (render-fn ctx state shape)))

;; REVIEW: This weird special dispatch on default feels pretty kludgy,
(defmethod render-fn :default
  [ctx state shape]
  (cond
    (sequential? shape)
    (render-all ctx (repeat state) shape)

    (contains? (set (keys (methods core/template-expand))) (:type shape))
      (render-fn ctx state (core/template-expand shape))

    :else
      (do
        (println (str "I don't know how to render a "
                      (or (:type shape) (type shape))))
        noop)))

(defn render
  "Returns a render function which when passed a context, renders the given
  shape."
  [ctx shape]
  (println shape)
  (.save ctx)
  (.setTransform ctx 1 0 0 1 0 0)
  (render-fn ctx {:style {} :zoom 1 :in-path? false} shape)
  (.restore ctx)
  (.beginPath ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal render logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn magnitude [a b c d]
  ;; HACK: This works for symmetric linear transforms, but as soon as we start
  ;; talking about skews and asymmetric scalings, it breaks down. I don't see
  ;; any way to manage this without writing my own pixel shaders. Hopefully I do
  ;; before I do.
  ;; !!!!!!!
  ;; Unless I ditch paths altogether and use bezier curves --- actually pairs of
  ;; curves --- to represent the edges of objects. They have no stroke, just a
  ;; fill, and so I can control exactly how thick the line is at all points. Soo
  ;; much work... But it has the potential to be a solution.
  (let [m (geometry/sqrt (geometry/det a b c d))]
    (if (geometry/nan? m)
      1
      m)))

(defmethod render-fn ::core/atx
  [ctx state {:keys [base-shape] {[a b c d] :matrix [e f] :translation} :atx}]
  (let [mag (magnitude a b c d)]
    (.save ctx)
    (.transform ctx a c b d e f)
    (render-fn ctx (update state :zoom * mag) base-shape)
    (.restore ctx)))

(defmethod render-fn ::core/path
  [ctx state {:keys [closed? contents style]}]
  (let [sub-state (-> state
                      (assoc :in-path? true)
                      (update :style #(merge style %)))
        ;; HACK: We need to make the first move explicitely despite the canvas
        ;; docs saying the first move is implicit...
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
    (render-all ctx (repeat sub-state) contents)
    (.restore ctx)))

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
