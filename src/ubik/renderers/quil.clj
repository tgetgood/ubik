(ns ubik.renderers.quil
  (:require [clojure.string :as string]
            [net.cgrand.macrovich :as macros]
            [quil.core :as q]
            [ubik.core :as core]
            [ubik.renderers.colours :as colours]
            [ubik.renderers.util :as util]
            [ubik.util :refer [implement-sequentials]])
  (:import
   [ubik.core AffineTransformation Arc Composite Frame Line
    RawText Region]))

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
        record-name (symbol (str "P" (name (core/type-case cmd))))]
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

  (text str x y))

(defrecord PUpdateStrokeWidth [w]
  HumanReadable
  (inspect [_]
    (str "Adjust stroke width by " w)))

(defn update-stroke-width [w]
  (PUpdateStrokeWidth. w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Stroke [r g b a])
(defrecord Fill [r g b a])
(defrecord Alpha [a])
(defrecord Font [f])

(defprotocol IStyle
  (process-style [this state]))

(extend-protocol IStyle
  Object
  (process-style [this stack]
    nil))

(defn styling-tx [xf]
  (let [stack (volatile! (list {:weight 1}))]
    (fn ([] (xf))
      ([acc] (xf acc))
      ([acc n]
       (let [out (process-style n stack)]
         (if out
           (reduce xf acc out)
           acc))))))

(defn read-stroke [style]
  (when-let [s (:stroke style)]
    (let [[r g b a] (colours/read-colour s)]
      [(Stroke. r g b (or a 255))])))

(defn read-fill [style]
  (when-let [f (:fill style)]
    (let [[r g b a] (colours/read-colour f)]
      [(Fill. r g b (or a 255))])))

(defn read-alpha [style]
  (when-let [alpha (:opacity style)]))

(defn read-font [style]
  nil)

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

(defprotocol IPathSegment
  (vertex-draw [this]))

(defn intern-region
  "Style is lexically captured in processing when defining shapes, so we can't
  actually intern shapes without global awareness of style."
  [boundary]
  )

(defn walk-compile [shape]
  (let [c (compile* shape)]
    (wrap-style (:style shape)
      (concat
       (:pre c)
       (:draw c)
       (when (:recur-on c)
         (walk-compile (:recur-on c)))
       (:post c)))))

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

  AffineTransformation
  (compile* [{{[a b c d] :matrix [e f] :translation} :atx base :base-shape}]
    (let [mag (util/magnitude a b c d)]
      {:pre [push-matrix
             push-style
             (apply-matrix a b e c d f)
             (update-stroke-width mag)]
       :recur-on base
       :post [pop-matrix
              pop-style]}))

  Composite
  (compile* [{:keys [style contents]}]
    {:style style
     :recur-on contents})

  Frame
  (compile* [{w :width h :height [x y] :corner base :base-shape}]
    {:pre [(clip x y w h)]
     :recur-on base
     :post [no-clip]})

  Region
  (compile* [{:keys [boundary style]}]
    #_(assert (every? #(satisfies? IPathSegment %) boundary))
    {:style style
     :pre []
     ;;:draw (intern-region boundary)
     :recur-on boundary
     :post []})

  Line
  (compile* [{[x1 y1] :from [x2 y2] :to style :style}]
    {:style style
     :draw [(line x1 y1 x2 y2)]})

  Arc
  (compile* [{r :radius [x y] :centre :keys [from to style clockwise?] :as this}]
    (let [d (+ r r)]
      {:style style
       :pre [push-style
             no-fill]
       :draw  [(arc x y d d from to)]
       :post [pop-style]}))

  RawText
  (compile* [{[x y] :corner t :text style :style}]
    {:style style
     :pre [push-style
           (fill 0 0 0 255)]
     :draw [(text t x y )]
     :post [pop-style]})

)

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
  (loop [state {:weight 1}
         [cmd & cmds] (walk-compile shape)]
    (when cmd
      (if (instance? PUpdateStrokeWidth cmd)
        (if (= 1 (:w cmd))
          (recur state cmds)
          (let [w (/ (:weight state) (:w cmd))]
            (invoke (stroke-weight w))
            (recur (assoc state :weight w) cmds)))
        (do
          (when (satisfies? Invocable cmd)
            (invoke cmd))
          (recur state cmds))))))

(defn debug [shape]
  (map inspect (walk-compile shape)))

(defn balanced-compile? [shape]
  (let [inst (walk-compile shape)]
    (= (count (filter #(= push-matrix %) inst))
       (count (filter #(= pop-matrix %) inst)))))

(require '[clojure.pprint :refer [pp pprint]])
