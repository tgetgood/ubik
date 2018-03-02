(ns ubik.renderers.canvas
  ;; TODO: Names in this space are shit. Product of experiments with
  ;; optimisation and lots of throwaway code.
  "HTML Canvas renderer. Technically an ad hoc compiler."
  (:require-macros [ubik.renderers.canvas
                    :refer [unsafe-invoke call compile-node compile-leaf
                            add-seq-compilers]])
  (:require [clojure.walk :as walk]
            [ubik.core :as core]
            [ubik.math :as math]
            [ubik.renderers.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Code Building
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord StrokeStyle [value sym])
(defrecord FillStyle [value sym])
(defrecord GlobalAlpha [value sym])
(defrecord Font [value sym])

(defrecord UnSetter [sym])

(defrecord LineWidthHack [mag])

(defrecord MaybeFill [])
(def *maybe-fill (MaybeFill.))

(defrecord Call [f arg1 arg2 arg3 arg4 arg5 arg6])

;; Use the same instance when we can to avoid allocating
(def *save (call "save"))
(def *restore (call "restore"))
(def *begin-path (call "beginPath"))
(def *stroke (call "stroke"))
(def *fill (call "fill"))
(def *clip (call "clip"))

;; TODO: Compilation as a notion needs to be first class. Geometry needs it
;; too. So move it to core?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti style-prop
  "Returns code for setting a single style property."
  (fn [[k v] s] k))

(defn process-gradient [m])

(defn process-colour [c]
  (cond
    (= :none c)  "rgba(0,0,0,0)"
    (keyword? c) (name c)
    (string? c)  c
    (map? c)     (process-gradient c)
    :else        "rgba(0,0,0,0)"))

(defmethod style-prop :stroke
  [[_ v] s]
  (let [c (process-colour v)]
    (when c
      (StrokeStyle. c s))))

(defmethod style-prop :fill
  [[_ v] s]
  (let [c (process-colour v)]
    (when c
      (FillStyle. c s))))

(defmethod style-prop :opacity
  [[_ v] s]
  (GlobalAlpha. v s))

(defmethod style-prop :font
  [[_ v] s]
  (Font. v s))

(defn simple-style [style sym]
  (when (seq style)
    (map #(style-prop % sym) style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Canvas Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Canvas2DRenderable
  (region-compile* [this])
  (compile* [this]))

(def compile (memoize compile*))

(add-seq-compilers Canvas2DRenderable
  PersistentVector
  ArrayList
  List
  LazySeq
  IndexedSeq
  IndexedSeqIterator)

(extend-protocol Canvas2DRenderable
  default
  (compile* [this]
    (if (core/template? this)
      (compile* (core/expand-template this))
      (do
        (println (str "I don't know how to render a " (type this) ". Aborting."))
        [])))

  nil
  (compile* [_]
    (println "Cannot render nil. Aborting.'")
    [])

  core/AffineTransformation
  (compile* [{{[a b c d] :matrix [e f] :translation} :atx base :base-shape}]
    (compile-node {:pre      [*save
                              (call "transform" a b c d e f)
                              (LineWidthHack. (util/magnitude a b c d))]
                   :recur-on base
                   :post     [*restore]}))

  core/Composite
  (compile* [{:keys [style contents]}]
    (compile-node {:style style :recur-on contents}))

  core/Frame
  (compile* [{w :width h :height [x y] :corner base :base-shape}]
    (compile-node {:pre      [*save
                              *begin-path
                              (call "rect" x y w h)
                              *clip]
                   :recur-on base
                   :post     [*restore]}))

  core/Region
  ;; Using canvas means that we can't style the boundary segments of a region
  ;; independently of the region itself. We can work around that with abstract
  ;; regions composed of a fill without boundary and a series of segments that
  ;; are styled but do not form a path.
  ;;
  ;; TODO: I should add that as a template.
  ;; FIXME: It doesn't need a template. This guy should render and fill an
  ;; invisible boundary and then render the boundary elements as is. That would
  ;; satisfy the contract.
  (compile* [{:keys [boundary style]}]
    (let [endpoints (map core/endpoints boundary)]
      (compile-leaf {:style style

                     :pre   [*begin-path]
                     :draw  (mapcat
                             (fn [seg [start _] [_ end]]
                               (if ( = start end)
                                 [seg]
                                 [(call "moveTo" (nth start 0) (nth start 1))
                                  seg]))
                             (map region-compile* boundary)
                             endpoints
                             (cons [nil nil] endpoints))
                     :post  [*maybe-fill
                             *stroke]})))

  core/RawText
  (compile* [{[x y] :corner :keys [style text]}]
    (compile-leaf {:draw [(call "fillText" text x y)]}))

  ;; Templates can implement protocols too
  core/Rectangle
  (compile* [{:keys [style width height] [x y] :corner}]
    (compile-leaf {:style style
                   :pre   [*begin-path]
                   :draw  [(call "rect" x y width height)]
                   :post  [*maybe-fill
                           *stroke]}))

  core/Line
  (region-compile* [{[x1 y1] :from [x2 y2] :to style :style}]
    (call "lineTo" x2 y2))
  (compile* [{[x1 y1] :from [x2 y2] :to style :style}]
    (compile-leaf {:style style
                   :pre   [*begin-path
                           (call "moveTo" x1 y1)]
                   :draw  [(call "lineTo" x2 y2)]
                   :post  [*stroke]}))

  core/Arc
  (region-compile* [{r :radius [x y] :centre :keys [from to style clockwise?]}]
    (call "arc" x y r from to (boolean clockwise?)))
  (compile* [{r :radius [x y] :centre :keys [from to style clockwise?] :as this}]
    (let [[x1 y1] (first (core/endpoints this))]
      (compile-leaf {:style style
                     :pre   [*begin-path
                             (call "moveTo" x1 y1)]
                     :draw  [(call "arc" x y r from to (boolean clockwise?))]
                     :post  [*stroke]})))

  core/Bezier
  (region-compile* [{[x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2}]
    (call "bezierCurveTo" cx1 cy1 cx2 cy2 x2 y2))
  (compile* [{[x1 y1] :from [x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2 :keys [style]}]
    (compile-leaf {:style style
                   :pre   [(call "moveTo" x1 y1)]
                   :draw  [(call "bezierCurveTo" cx1 cy1 cx2 cy2 x2 y2)]})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Execution Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Executable
  (exec [this ctx]
    "Execute this instruction on the context. Return value ignored."))

(extend-protocol Executable
  Call
  (exec [cmd ctx]
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
      "bezierCurveTo" 6))

  FillStyle
  (exec [cmd ctx]
    (unchecked-set ctx "fillStyle" (.-value cmd)))

  StrokeStyle
  (exec [cmd ctx]
    (unchecked-set ctx "strokeStyle" (.-value cmd)))

  GlobalAlpha
  (exec [cmd ctx]
    (unchecked-set ctx "globalAlpha" (.-value cmd)))

  Font
  (exec [cmd ctx]
    (unchecked-set ctx "font" (.-value cmd)))

  LineWidthHack
  (exec [cmd ctx]
    (unchecked-set ctx "lineWidth" (/ (.-lineWidth ctx) (.-mag cmd)))))

(defn exec-reduce
  "Returns a reducing function that runs exec on each element of coll on the
  given ctx."
  [ctx]
  (fn
    ([] nil)
    ([acc] nil)
    ([acc cmd] (exec ^not-native cmd ctx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Canvas State Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IState
  "Internal state store for the execution of canvas instructions."
  (clear-sym [this s])
  (set-fill [this s])
  (set-stroke [this s])
  (set-alpha [this s])
  (set-font [this s]))

(deftype CompilerState
    [^:mutable fill ^:mutable stroke ^:mutable alpha ^:mutable font]
  IState
  (clear-sym [this sym]
    (when (identical? sym fill)
      (set! fill nil))
    (when (identical? sym stroke)
      (set! stroke nil))
    (when (identical? sym font)
      (set! font nil))
    (when (identical? sym alpha)
      (set! alpha nil)))
  (set-fill [this s] (set! fill s))
  (set-stroke [this s] (set! stroke s))
  (set-alpha [this s] (set! alpha s))
  (set-font [this s] (set! font s)))

(defprotocol StackEmulator
  "It's a pretty barebones emulation"
  (stack-process [this xf acc state]))

(extend-protocol StackEmulator
  Call
  (stack-process [this xf acc _]
    (xf acc this))

  FillStyle
  (stack-process [this xf acc state]
    (if (nil? (.-fill state))
      (do
        (set-fill state (.-sym this))
        (xf acc this))
      acc))

  StrokeStyle
  (stack-process [this xf acc state]
    (if (nil? (.-stroke state))
      (do
        (set-stroke state (.-sym this))
        (xf acc this))
      acc))

  GlobalAlpha
  (stack-process [this xf acc state]
    (if (nil? (.-alpha state))
      (do
        (set-alpha state (.-sym this))
        (xf acc this))
      acc))

  Font
  (stack-process [this xf acc state]
    (if (nil? (.-font state))
      (do
        (set-font state (.-sym this))
        (xf acc this))
      acc))

 UnSetter
 (stack-process [this xf acc state]
   (clear-sym state (.-sym this))
   acc)

 LineWidthHack
 (stack-process [this xf acc _]
   (if ^boolean (js* "~{}===1" (.-mag this))
     acc
     (xf acc this)))

 MaybeFill
 (stack-process [this xf acc state]
   (if (nil? (.-fill state))
     acc
     (xf acc *fill))))

(defn stack-tx
  "Returns a tranducer that emulates the canvas API's statefullness and
  transforms the raw ubik instructions into final canvas instructions.
  Is it an interpreter or an optimising compiler? I don't know, it's a pretty
  crappy either...'"
  []
  (let [state (CompilerState. nil nil nil nil)]
    (fn [xf]
      (fn
        ([] (xf))
        ([acc] (xf acc))
        ([acc n]
         (stack-process ^not-native n xf acc state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Readable
  "Allows compiler internals to expose themselves as human readable
  instructions."
  (inspect [i] "Returns a string representing the instruction."))

(extend-protocol Readable
  UnSetter
  (inspect [i] nil)

  Call
  (inspect [i]
    (str
     (apply str "(." (:f i) " ctx"
            (interleave (repeat " ")
                        (remove nil? [(:arg1 i) (:arg2 i) (:arg3 i)
                                      (:arg4 i) (:arg5 i) (:arg6 i)])))
     ")"))

  LineWidthHack
  (inspect [i]
    (str "(set! (.-lineWidth ctx) (/ (.-lineWidth ctx) " (:mag i) ")"))

  default
  (inspect [i] i))

(defn code
  "Returns a vector of strings which represents the code that gets executed on
  the canvas context in attempting to draw the given shape."
  [shape]
  (transduce (comp (stack-tx) (map inspect) (remove nil?))
             conj
             (compile shape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Main Draw Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- clear-screen!
  "Clear the rendering context's canvas."
  [ctx]
  (.clearRect ctx 0 0 (-> ctx .-canvas .-width) (-> ctx .-canvas .-height)))

(defn- context
  "Returns the 2d rendering context for the given HTML canvas DOM element."
  [elem]
  (.getContext elem "2d"))

(defn execute! [ctx cmds]
  (clear-screen! ctx)
  (transduce (stack-tx) (exec-reduce ctx) cmds))

(defn draw!
  "Draw world to HTML Canvas element."
  [canvas-element world]
  (execute! (context canvas-element) (compile world)))
