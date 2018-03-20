(ns ubik.renderers.quil
  #?@(:clj
       [(:require
         [clojure.string :as string]
         [net.cgrand.macrovich :as macros]
         [quil.core :as q]
         [ubik.core :as core]
         [ubik.renderers.colours :as colours]
         [ubik.renderers.util :as util])
        (:import
         [ubik.core
          AffineTransformation
          Arc
          Composite
          Frame
          Line
          RawText
          Region])]
       :cljs
       [(:require
         [clojure.string :as string]
         [net.cgrand.macrovich :as macros]
         [quil.core :as q]
         [ubik.core :as core]
         [ubik.renderers.util :as util])
        (:require-macros
         [ubik.renderers.quil :refer [defcmds implement-sequentials]])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Quil Wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HumanReadable
  (inspect [this]))

(defprotocol Invocable
  (invoke [this]))

(macros/deftime

  (defn tocmd [cmd]
    (symbol "quil.core" (name cmd))
    #_(let [parts (string/split (name cmd) #"-")]
      (symbol (apply str "." (first parts)
                     (map string/capitalize (rest parts))))))

  (defn call-form [g cmd args]
    (apply list (tocmd cmd) g args))

  (defn cmd-body [form]
    (let [single? (symbol? form)
          cmd (if single? form (first form))
          args (if single? [] (rest form))
          record-name (symbol (str "P" (name (core/type-case cmd))))
          graphics (gensym)]
      `(do
         (defrecord ~record-name [~@args]
           HumanReadable
           (inspect [_#]
             (str (apply str ~(subs (name (tocmd cmd)) 1) "("
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

  (defmacro implement-sequentials
    {:style/indent [1 :form [1]]}
    [prot & methods]
    (let [types (macros/case :cljs '[List
                                     LazySeq
                                     PersistentVector
                                     IndexedSeq
                                     ArrayList]
                             :clj '[clojure.lang.PersistentVector
                                    clojure.lang.PersistentList
                                    clojure.lang.ArraySeq
                                    clojure.lang.IndexedSeq
                                    clojure.lang.PersistentVector$ChunkedSeq
                                    clojure.lang.LazySeq])]
      `(extend-protocol ~prot
         ~@(mapcat (fn [a b] `[~a ~@b]) types (repeat methods))))))

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

(defrecord PUpdateStrokeWidth [w])

(defn update-stroke-width [w]
  (PUpdateStrokeWidth. w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-stroke [style]
  (when-let [s (:stroke style)]
    (let [[r g b] (colours/read-colour s)]
      [(stroke r g b 255)])))

(defn wrap-style
  {:style/indent 1}
  [style cmds]
  (if style
    (concat
     [push-style]
     (read-stroke style)
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

(implement-sequentials QuilRenderable
  (compile* [this]
    {:draw (mapcat walk-compile this)}))

(extend-protocol QuilRenderable
  nil
  (compile* [_]
    #_(println "Can't render nil.")
    [])

  #?(:clj Object :cljs default)
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
    {:style style
     :pre []
     :recur-on boundary
     :post []})

  Line
  (compile* [{[x1 y1] :from [x2 y2] :to style :style}]
    {:style style
     :draw [(line x1 y1 x2 y2)]})

  Arc
  (compile* [{r :radius [x y] :centre :keys [from to style clockwise?] :as this}]
    {:style style
     :pre [push-style
           no-fill]
     :draw  [(arc x y r r from to)]
     :post [pop-style]})

  RawText
  (compile* [{[x y] :corner t :text style :style}]
    {:style style
     :draw [(text t x y )]})

)

(defonce t (atom nil))

(defn renderer
  "Returns a render function which when passed a context, renders the given
  shape."
  [graphics shape]
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
          (invoke cmd)
          (recur state cmds))))))

(defn debug [shape]
  (map inspect (walk-compile shape)))

(defn balanced-compile? [shape]
  (let [inst (walk-compile shape)]
    (= (count (filter #(= push-matrix %) inst))
       (count (filter #(= pop-matrix %) inst)))))

(require '[clojure.pprint :refer [pp pprint]])
