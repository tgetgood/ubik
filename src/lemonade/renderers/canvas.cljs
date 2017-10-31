(ns lemonade.renderers.canvas
  "Render shapes to HTML Canvas."
  (:require [clojure.spec.alpha :as s]
            [lemonade.core :as core]
            [lemonade.geometry :as geometry])
  (:require-macros [lemonade.renderers.canvas :refer [with-path-style]]))

(comment "Uses a half baked CPS transform so that some of the work can be done
  at compile time if the shape is statically known. Not sure how much inlining
  the CLJS compiler (Closure?) does, but there's potential. Doesn't actually do
  anything at compile time.")
;; REVIEW: Diabled

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private noop
  "What a render-fn returns if it wants to do nothing."
  (constantly nil))

(def default-style
  "Default style of images in lemonade."
  {:stroke  {:width   0
             :colour  :black
             :dash    []
             :ends    :butt
             :corners :mitre}
   :fill    :none
   :opacity 1
   :font    "sans serif 10px"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- flatten-style
  "Converts user input style to internal renderer style format"
  [style]
  (let [stroke (:stroke style)]
    (merge (dissoc style :stroke)
           (cond
             (map? stroke) stroke
             (keyword? stroke) {:colour stroke}
             (string? stroke) {:colour stroke}
             :else {}))))

(defmulti style-ctx (fn [state [k v]] k))

(defn safe-style-1 [{:keys [style] :as state} [k v]]
  (if (and (contains? style k) (not= (get style k) v))
      noop
      (style-ctx state [k v])))

(defn safe-style [state style]
  (let [style (merge (flatten-style default-style) (flatten-style style))]
    (if (empty? style)
      noop
      (apply juxt (map (partial safe-style-1 state) style)))))

(defmethod style-ctx :width
  [{:keys [zoom]} [_ v]]
  (let [w (if (zero? v) (/ 1 zoom) v)]
    (fn [ctx] (aset ctx "lineWidth" w))))

(defn process-gradient [m])

(defn process-colour [c]
  (cond
    (= :none c)  "rgba(0,0,0,0)"
    (keyword? c) (name c)
    (string? c)  c
    (map? c)     (process-gradient c)
    :else        nil))

(defmethod style-ctx :colour
  [_ [_ v]]
  (let [c (process-colour v)]
    (if-not c
      noop
      (fn [ctx]
        (aset ctx "strokeStyle" (if (fn? c) (c ctx) c))))))

(defmethod style-ctx :dash
  [_ [_ v]]
  #(.setLineDash % (clj->js v)))

(defmethod style-ctx :ends
  [_ [_ v]]
  #(aset % "lineCap" (name v)))

(defmethod style-ctx :corners
  [_ [_ v]]
  #(aset % "lineJoin" (name v)))

(defmethod style-ctx :fill
  [_ [_ v]]
  (let [c (process-colour v)]
    (if-not c
      noop
      #(aset % "fillStyle" (if (fn? c) (c %) c)))))

(defmethod style-ctx :opacity
  [_ [_ v]]
  #(aset % "globalAlpha" v))

(defmethod style-ctx :font
  [_ [_ v]]
  #(aset % "font" v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti render-fn (fn [state shape] (:type shape)))

(defn- render-all [state shapes]
  (if (empty? shapes)
    noop
    (apply juxt (map (partial render-fn state) shapes))))

;; REVIEW: This weird special dispatch on default feels pretty kludgy,
(defmethod render-fn :default
  [state x]
  (cond
    (sequential? x)
    (render-all state x)

    (contains? (set (keys (methods core/template-expand))) (:type x))
      (render-fn state (core/template-expand x))

    :else
      (do
        (println (str "I don't know how to render a " (:type x)))
        noop)))

(defn renderer
  "Returns a render function which when passed a context, renders the given
  shape."
  [shape]
  (let [cont (render-fn {:style {} :zoom 1 :in-path? false} shape)]
    #(doto %
       .save
       (.setTransform 1 0 0 1 0 0)
       cont
       .restore
       .beginPath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal render logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-atx [{[a b c d] :matrix [e f] :translation}]
  (fn [ctx]
    (.transform ctx a c b d e f)))

(defn magnitude [atx]
  ;; HACK: This works for symmetric linear transforms, but as soon as we start
  ;; talking about skews and asymmetric scalings, it breaks down. I don't see
  ;; any way to manage this without writing my own pixel shaders. Hopefully I do
  ;; before I do.
  ;; !!!!!!!
  ;; Unless I ditch paths altogether and use bezier curves --- actually pairs of
  ;; curves --- to represent the edges of objects. They have no stroke, just a
  ;; fill, and so I can control exactly how thick the line is at all points. Soo
  ;; much work... But it has the potential to be a solution.
  (let [m (js/Math.sqrt (apply geometry/det (:matrix atx)))]
    (if (js/isNaN m)
      1
      m)))

(defmethod render-fn ::core/atx
  [state {:keys [base-shape atx]}]
  (let [tx   (apply-atx atx)
        mag (magnitude atx)
        cont (render-fn (update state :zoom * mag) base-shape)]
    #(doto %
       .save
       tx
       cont
       .restore)))

(defmethod render-fn ::core/path
  [state {:keys [closed? contents style]}]
  (if (empty? contents)
    noop
    (let [sub-state (-> state
                        (assoc :in-path? true)
                        (update :style merge (flatten-style style)))
          cont (render-all sub-state contents)
          stylise (safe-style state style)]
      (fn [ctx]
        (.save ctx)
        (.beginPath ctx)
        (stylise ctx)
        (cont ctx)
        (when closed?
          (.closePath ctx)
          (let [fill (-> sub-state :style :fill)]
            (when-not (or (nil? fill) (= :none fill))
              (.fill ctx))))
        (.stroke ctx)
        (.restore ctx)))))

(defmethod render-fn ::core/composite
  [state {:keys [style contents]}]
  (let [cont (render-all (update state :style merge (flatten-style style))
                         contents)
        stylise (safe-style state style)]
    #(doto %
       .save
       stylise
       cont
       .restore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Leaf renderers
;;
;; At some point we have to render something, Less and less though, it would
;; appear.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-fn ::core/arc
  [state {[x y] :centre r :radius :keys [from to style clockwise?] :as arc}]
  (println (meta arc))
  (with-path-style (assoc state :override (:jump (meta arc))) style [(+ x r) y]
    (.arc x y r from to (boolean clockwise?))))

(defmethod render-fn ::core/line
  [state {:keys [from to style] :as o}]
  (with-path-style state style from
    (.lineTo (first to) (second to))))

(defmethod render-fn ::core/bezier
  [state {[x1 y1] :from [x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2 style :style}]
  (with-path-style state style [x1 y1]
    (.bezierCurveTo cx1 cy1 cx2 cy2 x2 y2)))
