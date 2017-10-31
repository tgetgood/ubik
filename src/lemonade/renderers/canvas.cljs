(ns lemonade.renderers.canvas
  "Render shapes to HTML Canvas."
  (:require [clojure.spec.alpha :as s]
            [lemonade.core :as core]
            [lemonade.geometry :as geometry])
  (:require-macros [lemonade.renderers.canvas :refer [with-style with-style*]]))

(comment "Uses a half baked CPS transform so that some of the work can be done
  at compile time if the shape is statically known. Not sure how much inlining
  the CLJS compiler (Closure?) does, but there's potential. Doesn't actually do
  anything at compile time.")
;; REVIEW: Diabled

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Setup
;;
;; Ubiquitous use of dynamic environments. Is this clever or too clever?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def noop (constantly nil))

(def ^:dynamic ^:private *in-path?*
  "Tracks whether we are currently inside a path (by force aligning the canvas
  sense to the lemonade sense of the term)."
  false)

(def ^:dynamic ^:private *zoom*
  "Current zoom level"
  1)

(def ^:dynamic ^:private *style*
  "Current style at this point in the render process."
  {})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- flatten-style
  "Converts user input style to internal renderer style format"
  [style]
  (merge (dissoc style :stroke) (:stroke style)))

(def default-style
  {:stroke  {:width   0
                  :colour  :black
                  :dash  []
                  :ends :square
                  :corners :mitre}
   :fill    :none
   :opacity 1
   :font    "sans serif 10px"})

(defmulti style-ctx (fn [[k v]] k))

(defmethod style-ctx :width
  [[_ v]]
  (let [w (if (zero? v) (/ 1 *zoom*) v)]
    (fn [ctx] (aset ctx "lineWidth" v))))

(defn process-gradient [m])

(defn process-colour [c]
  (cond
    (= :none c)  nil
    (keyword? c) (name c)
    (string? c)  c
    (map? c)     (process-gradient c)
    :else        nil))

(defmethod style-ctx :colour
  [[_ v]]
  (let [c (process-colour v)]
    (if-not c
      noop
      (fn [ctx]
        (aset ctx "strokeStyle" (if (fn? c) (c ctx) c))))))

(defmethod style-ctx :dash
  [[_ v]]
  #(.setLineDash % (clj->js v)))

(defmethod style-ctx :ends
  [[_ v]]
  #(aset % "lineCap" (name v)))

(defmethod style-ctx :corners
  [[_ v]]
  #(aset % "lineJoin" (name v)))

(defmethod style-ctx :fill
  [[_ v]]
  (let [c (process-colour v)]
    (if-not c
      noop
      #(aset % "fillStyle" (if (fn? c) (c %) c)))))

(defmethod style-ctx :opacity
  [[_ v]]
  #(aset % "globalAlpha" v))

(defmethod style-ctx :font
  [[_ v]]
  #(aset % "font" v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti render-fn :type)

;; REVIEW: This weird special dispatch on default feels pretty kludgy,
(defmethod render-fn :default
  [x]
  (cond
    (sequential? x)
      (apply juxt (map render-fn x))

    (contains? (set (keys (methods core/template-expand))) (:type x))
      (render-fn (core/template-expand x))

    :else
      (do
        (println (str "I don't know how to render a " (:type x)))
        noop)))

(defn renderer
  "Returns a render function which when passed a context, renders the given
  shape."
  [shape]
  (let [cont (render-fn shape)]
    ;; Just wrap the render fn in some state guarding. Ideally we want to be
    ;; able to insert our code into an existing canvas app without messing it up
    ;; or being messed up by it. Let's see how that goes...
    (with-style default-style
      (.setTransform 1 0 0 1 0 0)
      cont
      .beginPath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal render logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-atx [{[a b c d] :matrix [e f] :translation}]
  (fn [ctx]
    (.transform ctx a c b d e f)))

(defmethod render-fn ::core/atx
  [{:keys [base-shape atx]}]
  (let [tx   (apply-atx atx)
        cont (render-fn base-shape)]
    (with-style {}
      tx
      cont)))

(defmethod render-fn ::core/path
  [{:keys [closed? contents style]}]
  (if (empty? contents)
    noop
    (let [cont (apply juxt (map render-fn contents))]
      (with-style* style
        (fn [ctx]
          (.beginPath ctx)
          (binding [*in-path?* true]
            (cont ctx))
          (when closed?
            (.closePath ctx)
            (when (and (:fill *style*) (not= (:fill *style*) :none))
              (.fill ctx)))
          (.stroke ctx))))))

(defmethod render-fn ::core/composite
  [{:keys [style contents]}]
  (let [cont (apply juxt (map render-fn contents))]
    (with-style style cont)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Leaf renderers
;;
;; At some point we have to render something, Less and less though, it would
;; appear.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn ::core/arc
  [{[x y] :centre r :radius :keys [from to style clockwise?]}]
  (fn [ctx]
    (.moveTo ctx (+ x r) y)
    (.arc ctx x y r from to (boolean clockwise?))
    (when-not *in-path?*
      (.stroke ctx))))

(defmethod render-fn ::core/line
  [{:keys [from to style] :as o}]
  (fn [ctx]
    (when-not *in-path?*
      (.beginPath ctx))
    (.moveTo ctx (first from) (second from))
    (.lineTo ctx (first to) (second to))
    (when-not *in-path?*
      (.stroke ctx))))

(defmethod render-fn ::core/bezier
  [{[x1 y1] :from [x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2}]
  (fn [ctx]
    (.moveTo ctx x1 y1)
    (.bezierCurveTo ctx cx1 cy1 cx2 cy2 x2 y2)
    (when-not *in-path?*
      (.stroke ctx))))
