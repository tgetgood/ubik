(ns lemonade.renderers.canvas
  "HTML Canvas renderer. Technically an ad hoc compiler."
  (:require-macros [lemonade.renderers.canvas :refer [unsafe-invoke]])
  (:require [lemonade.core :as core]
            [lemonade.renderers.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Code Building
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ctx-sym (str "ctx_G1" ))

(defn encode [x]
  (if (string? x)
    (str "'" x "'")
    (str x)))

(defn setter [prop value]
  (array "set" prop value))

(defn ^boolean setter? [o]
  (identical? "set" (unchecked-get o 0)))

(defn call [f & args]
  (apply array f args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti style-prop
  "Returns code for setting a single style property."
  (fn [state [k v]] k))

(defn style-wrapper
  "Returns a minimal sequence of instructions for transitioning from the current
  global style (stored in state) to the desired style."
  [state style]
  (let [parent-style (:style state)
        child-style (merge {:stroke :black} style)]
    (->> child-style
         (filter (fn [[k v]] (or (not (contains? parent-style k))
                                (= (get parent-style k) v))))
         (mapcat #(style-prop state %)))))

(defn process-gradient [m])

(defn process-colour [c]
  (cond
    (= :none c)  "rgba(0,0,0,0)"
    (keyword? c) (name c)
    (string? c)  c
    (map? c)     (process-gradient c)
    :else        "rgba(0,0,0,0)"))

(defmethod style-prop :stroke
  [{:keys [zoom]} [_ v]]
  (let [c (process-colour v)]
    (when c
      [(setter "lineWidth" (/ 1 zoom))
       (setter "strokeStyle" c)])))

(defmethod style-prop :fill
  [_ [_ v]]
  (let [c (process-colour v)]
    (when c
      [(setter "fillStyle" c)])))

(defmethod style-prop :opacity
  [_ [_ v]]
  [(setter "globalAlpha" v)])

(defmethod style-prop :font
  [_ [_ v]]
  [(setter "font" v)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn join [lists]
  (apply concat (remove empty? lists)))

(defn safely [& lists]
  (concat [(call "save")]
          (join lists)
          [(call "restore")]))

(defn with-style [state style & cmds]
  (let [styles (style-wrapper state style)]
    (if (empty? styles)
      (join cmds)
      (apply safely styles cmds))))

(defn path-wrapper [state style [x y] & lists]
  (let [in? (:in-path? state)]
    (with-style state style
      (when-not in?
        [(call "beginPath")])
      (when (or (:override state) (not in?))
        [(call "moveTo" x y)])
      (join lists)
      (when-not in?
        [(call "stroke")]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Canvas Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Canvas2DRenderable
  (compile-renderer [this state]
    "Returns instructions for rendering this a ctx."))

(extend-protocol Canvas2DRenderable
  default
  (compile-renderer [this state]
    (if (core/template? this)
      (compile-renderer (core/expand-template this) state)
      (println (str "I don't know how to render a " this ". Aborting."))))

  nil
  (compile-renderer [_ _]
    (println "Cannot render nil. Aborting.'"))

  core/AffineTransformation
  (compile-renderer [{{[a b c d] :matrix [e f] :translation} :atx
                      base                                   :base-shape}
                     state]
    (safely [(call "transform" a b c d e f)]
            (compile-renderer base (update state :zoom *
                                           (util/magnitude a b c d)))))

  PersistentVector
  (compile-renderer [shapes state]
    (mapcat compile-renderer shapes (repeat state)))

  List
  (compile-renderer [shapes state]
    (mapcat compile-renderer shapes (repeat state)))

  LazySeq
  (compile-renderer [shapes state]
    (mapcat compile-renderer shapes (repeat state)))

  core/Composite
  (compile-renderer [{:keys [style contents]} state]
    (with-style state style
      (mapcat compile-renderer
              contents
              (repeat (update state :style #(merge style %))))))

  core/Frame
  (compile-renderer [{base :base-shape w :width h :height [x y] :corner} state]
    (safely [(call "beginPath")
             (call "rect" x y w h)
             (call "clip")]
            (compile-renderer base state)))

  core/Region
  (compile-renderer [{:keys [style boundary]} state]
    (let [substate  (-> state
                        (assoc :in-path? true)
                        (update :style #(merge style %)))
          substates (cons (assoc substate :override true)
                          (repeat substate))]
      (with-style state style
        [(call "beginPath")]
        (mapcat compile-renderer boundary substates)
        [(call "closePath")]
        (when-let [fill (-> substate :style :fill)]
          (when (not= fill :none)
            [(call "fill")]))
        [(call "stroke")])))

  core/Line
  (compile-renderer [{[x y] :to :keys [from style]} state]
    (path-wrapper state style from
      [(call "lineTo" x y)]))

  core/Arc
  (compile-renderer [arc state]
    (let [{r :radius [x y] :centre :keys [from to style clockwise?]} arc]
      (path-wrapper
        (assoc state :override (:jump (meta arc)))
        style
        [(+ x r) y]
        [(call "arc" x y r from to (boolean clockwise?))])))

  core/Bezier
  (compile-renderer [{[x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2 :keys [style from]}
                     state]
    (path-wrapper state style from
                  [(call "bezierCurveTo" cx1 cy1 cx2 cy2 x2 y2)]))

  core/RawText
  (compile-renderer [{[x y] :corner :keys [style text]} state]
    (with-style state style
      [(call "fillText" text x y)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-render-state {:style {} :zoom 1 :in-path? false})

(defn- clear-screen!
  "Clear the rendering context's canvas."
  [ctx]
  (.clearRect ctx 0 0 (-> ctx .-canvas .-width) (-> ctx .-canvas .-height)))

(defn- context
  "Returns the 2d rendering context for the given HTML canvas DOM element."
  [elem]
  (.getContext elem "2d"))

(defn- execute!
  "Given a sequence of rendering operations and a context, carry them out"
  [ctx cmd]
  (if (setter? cmd)
    (unchecked-set ctx (unchecked-get cmd 1) (unchecked-get cmd 2))
    (unsafe-invoke ctx cmd
      "save" 0
      "restore" 0
      "transform" 6
      "lineTo" 2
      "arc" 6
      "stroke" 0
      "fill" 0
      "fillText" 3
      "beginPath" 0
      "closePath" 0
      "moveTo" 2
      "rect" 4
      "clip" 0
      "bezierCurveTo" 6)))

(defn executor [ctx cmds]
  (clear-screen! ctx)
  (run! (partial execute! ctx) cmds))

(defn draw!
  "Draw world to HTML Canvas element."
  [canvas-element world]
  (let [cmds (compile-renderer world default-render-state)]
    (executor (context canvas-element) cmds)))
