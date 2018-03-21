(ns ubik.renderers.quil
  (:require [clojure.string :as string]
            [net.cgrand.macrovich :as macros]
            [quil.core :as q]
            [ubik.core :as core]
            [ubik.renderers.colours :as colours]
            [ubik.renderers.util :as util]
            [ubik.util :refer [implement-sequentials]]
            [ubik.math :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Quil Wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HumanReadable
  (inspect [this]))

(defprotocol Invocable
  (invoke [this]))

(defn tocmd [cmd]
  @(get (ns-interns 'quil.core) cmd))

(defn processing-name [cmd]
  (let [parts (string/split (name cmd) #"-")]
    (apply str (first parts)
           (map string/capitalize (rest parts)))))

(defn cmd-body [form]
  (let [single? (symbol? form)
        cmd (if single? form (first form))
        args (if single? [] (rest form))
        record-name (symbol (str "Quil" (name (core/type-case cmd))))]
    `(do
       (defrecord ~record-name [~@args]
         HumanReadable
         (inspect [_#]
           (str (apply str ~(processing-name cmd) "("
                       (interpose ", " [~@args]))
                ")"))
         Invocable
         (invoke [_#]
           ~(apply list (tocmd cmd) args)))
       ~(if single?
          `(def ~cmd (~(symbol (str record-name "."))))
          `(defn ~cmd [~@args]
             (~(symbol (str record-name ".")) ~@args))))))

(defmacro defcmds [& forms]
  `(do
     ~@(map cmd-body forms)))

(defcmds
  push-matrix
  pop-matrix
  (apply-matrix a b c d e f)
  (rotate a)
  (translate x y)

  begin-shape
  end-shape
  begin-contour
  end-contour
  (vertex x y)
  (bezier-vertex cx1 cy1 cx2 cy2 x y)

  push-style
  pop-style
  (stroke-weight w)
  (stroke r g b a)
  (fill r g b a)
  no-fill
  no-stroke

  (clip x y w h)
  no-clip

  (line x1 y1 x2 y2)
  (arc x y a b from to)
  (bezier x1 y1 c1x c1y c2x c2y x2 y2)

  (ellipse x y w h)

  (text str x y)
  (text-font f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling. Mostly hackery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IStyle
  (process-style [this state xf acc]))

(defrecord UpdateStrokeWidth [w]
  IStyle
  (process-style [_ state xf acc]
    (if (= w 1)
      [state acc]
      (let [w' (/ (:weight @state) w)]
        (vswap! state assoc :weight w')
        (xf acc (stroke-weight w'))))))

(defn update-stroke-width [w]
  (UpdateStrokeWidth. w))

(defrecord Stroke [r g b]
  IStyle
  (process-style [_ state xf acc]
    (if (:stroke @state)
      acc
      (let [a (get @state :opacity 255)]
        (vswap! state assoc :stroke [r g b])
        (xf acc (stroke r g b a))))))

(defrecord NoStroke []
    IStyle
  (process-style [_ state xf acc]
    (if (:stroke @state)
      acc
      (do
        (vswap! state assoc :stroke :none)
        (xf acc no-stroke)))))

(defrecord Fill [r g b]
  IStyle
  (process-style [_ state xf acc]
    (if (:fill @state)
      acc
      (let [a (get @state :opacity 255)]
        (vswap! state assoc :fill [r g b])
        (xf acc (fill r g b a))))))

(defrecord NoFill []
  IStyle
  (process-style [_ state xf acc]
    (if (:fill @state)
      acc
      (do
        (vswap! state assoc :fill :none)
        (xf acc no-fill)))))

(defrecord Opacity [a]
  ;; REVIEW: Should opacity stack or override? That is should nested setting of
  ;; partial transparency lead to a more transparent image, or should the top
  ;; setting win? Currently the latter because that's the general semantics of
  ;; styling...
  IStyle
  (process-style [_ state xf acc]
    (if (:opacity @state)
      acc
      (let [o (math/floor (* a 255))]
        (vswap! state assoc :opacity o)
        (let [acc (if (and (:stroke @state) (not= :none (:stroke @state)))
                    (let [[r g b] (:stroke @state)]
                      (xf acc (stroke r g b o)))
                    acc)
              acc (if (and (:fill @state) (not= :none (:fill @state)))
                      (let [[r g b] (:fill @state)]
                        (xf acc (fill r g b o)))
                    acc)]
          acc)))))

(defrecord Font [f]
  IStyle
  (process-style [_ state xf acc]
    (if (:font @state)
      acc
      (do
        (vswap! state assoc :font f)
        ;; TODO: stub
        #_(xf acc (text-font f))))))

(extend-protocol IStyle
  Object
  (process-style [this state xf acc]
    (xf acc this)))

(defn style-stack-tx [xf]
  (let [stack (volatile! (list ))
        state (volatile! {:weight 1})]
    (fn ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (cond
         (= n push-style) (vreset! stack (cons @state @stack))
         (= n pop-style) (do (vreset! state (first @stack))
                             (vreset! stack (rest @stack)))
         :else nil)
       ;; HACK: I can't say I'm a fan of passing a volatile off into a generic
       ;; function to be mutated... We do have single threaded semantics, so
       ;; races aren't a problem, but it just feels icky.
       (process-style n state xf acc)))))

;; REVIEW: So repetitive...
(defn read-stroke [style]
  (when-let [s (:stroke style)]
    (if (= s :none)
      [(NoStroke.)]
      (let [[r g b] (colours/read-colour s)]
        [(Stroke. r g b)]))))

(defn read-fill [style]
  (when-let [f (:fill style)]
    (if (= f :none)
      [(NoFill.)]
      (let [[r g b] (colours/read-colour f)]
        [(Fill. r g b)]))))

(defn read-alpha [style]
  (when-let [alpha (:opacity style)]
    [(Opacity. alpha)]))

(defn read-font [style]
  (when-let [font (:font style)]
    [(Font. font)]))

(defn wrap-style
  {:style/indent 1}
  [style cmds]
  (if style
    (concat
     [push-style]
     (read-stroke style)
     (read-fill style)
     (read-alpha style)
     (read-font style)
     cmds
     [pop-style])
    cmds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol QuilRenderable
  (compile* [this]))

(defn walk-compile [shape]
  (let [c (compile* shape)]
    (wrap-style (:style shape)
      (concat
       (:pre c)
       (:draw c)
       (when (:recur-on c)
         (walk-compile (:recur-on c)))
       (:post c)))))

(defprotocol IPathSegment
  (vertex-draw [this]))

(defn intern-shape
  "Style is lexically captured in processing when defining shapes, so we can't
  actually intern shapes without global awareness of style."
  [boundary]
  )

(defn circle-bezier
  "Draws an approximation to a circle as 4 bezier curve segments."
  ([r] (circle-bezier r false))
  ([r clockwise?]
   (let [c 0.551915024494
         -r (- r)
         rc (* r c)
         -rc (* -1 r c)]
     (if clockwise?
       [(vertex r 0)
        (bezier-vertex r -rc rc -r 0 -r)
        (bezier-vertex -rc -r -r -rc -r 0)
        (bezier-vertex -r rc -rc r 0 r)
        (bezier-vertex rc r r rc r 0)]
       [(vertex r 0)
        (bezier-vertex r rc rc r 0 r)
        (bezier-vertex -rc r -r rc -r 0)
        (bezier-vertex -r -rc -rc -r 0 -r)
        (bezier-vertex rc -r r -rc r 0)]))))

(implement-sequentials QuilRenderable
  (compile* [this]
    {:draw (mapcat walk-compile this)}))

(extend-protocol QuilRenderable
  nil
  (compile* [_]
    (println "Can't render nil.")
    [])

  Object
  (compile* [this]
    (if (core/template? this)
      (compile* (core/expand-template this))
      (do
        ;; FIXME: We need logging, but logging these to stdout every frame is no good.
        (println (str "I don't know how to render a " (type this)
                      ". Doing nothing."))
       [])))

  ubik.core.AffineTransformation
  (compile* [{{[a b c d] :matrix [e f] :translation} :atx base :base-shape}]
    (let [mag (util/magnitude a b c d)]
      {:pre      [push-matrix
                  push-style
                  (apply-matrix a b e c d f)
                  (update-stroke-width mag)]
       :recur-on base
       :post     [pop-matrix
                  pop-style]}))

  ubik.core.Composite
  (compile* [{:keys [style contents]}]
    {:style    style
     :recur-on contents})

  ubik.core.Frame
  (compile* [{w :width h :height [x y] :corner base :base-shape}]
    {:pre      [(clip x y w h)]
     :recur-on base
     :post     [no-clip]})

  ubik.core.Region
  (compile* [{:keys [boundary style]}]
    #_(assert (every? #(satisfies? IPathSegment %) boundary))
    {:style    style
     :pre      []
     ;;:draw (intern-region boundary)
     :recur-on boundary
     :post     []})

  ;;; HACK: Override basic regions so that we don't have to solve this problem
  ;;; yet.
  ubik.core.Polyline
  (compile* [{:keys [style points]}]
    {:style style
     :draw  (concat
             [begin-shape]
             (map (fn [[x y]] (vertex x y)) points)
             [end-shape])})

  ubik.core.Circle
  (compile* [{[x y] :centre r :radius style :style}]
    (let [d (+ r r)]
      {:style style
       :draw  [(ellipse x y d d)]}))

  ubik.core.Annulus
  (compile* [{[x y] :centre r1 :inner-radius r2 :outer-radius style :style}]
    {:style style
     :pre [push-matrix
           (translate x y)]
     :draw  (concat
             [begin-shape]
             (circle-bezier r2)
             ;; To go on to shape subtraction, I'm going to need a first class
             ;; notion of a spline so that I can reverse the path for the sake
             ;; of the winding number algo embedded in processing.
             [end-contour
              begin-contour]
             (circle-bezier r1 true)
             [end-shape])
     :post [pop-matrix]})

  ubik.core.Line
  (compile* [{[x1 y1] :from [x2 y2] :to style :style}]
    {:style style
     :draw  [(line x1 y1 x2 y2)]})

  ubik.core.Arc
  (compile* [{r :radius [x y] :centre :keys [from to style clockwise?] :as this}]
    (let [d (+ r r)]
      {:style style
       :pre   [push-style
               no-fill]
       :draw  [(arc x y d d from to)]
       :post  [pop-style]}))

  ubik.core.RawText
  (compile* [{[x y] :corner t :text style :style}]
    {:style style
     :pre   [push-style
             (fill 0 0 0 255)]
     :draw  [(text t x y )]
     :post  [pop-style]}))

(defonce t (atom nil))

(defn renderer
  "Returns a render function which when passed a context, renders the given
  shape."
  [^quil.Applet applet shape]
  ;; Need to set default fill to 0 otherwise text won't render and you won't
  ;; know why...
  (q/clear)
  (q/reset-matrix)
  (q/background 255)
  (q/stroke 0)
  (q/no-fill)
  (loop [state {:weight 1}
         [cmd & cmds] (eduction style-stack-tx (walk-compile shape))]
    (when cmd
      (when (satisfies? Invocable cmd)
        (invoke cmd))
      (recur state cmds))))

(defn debug [shape]
  (into [] (eduction (comp style-stack-tx (map inspect)) (walk-compile shape))))

(defn balanced-compile? [shape]
  (let [inst (walk-compile shape)]
    (= (count (filter #(= push-matrix %) inst))
       (count (filter #(= pop-matrix %) inst)))))

(require '[clojure.pprint :refer [pp pprint]])
