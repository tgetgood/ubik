(ns ubik.renderers.quil
  (:require [clojure.string :as string]
            [net.cgrand.macrovich :as macros]
            [quil.core :as q]
            [ubik.core :as core]
            [ubik.renderers.util :as util])
  (:import [ubik.core AffineTransformation Line]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Quil Wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HumanReadable
  (inspect [this]))

(defprotocol Invocable
  (invoke [this graphics]))

(macros/deftime

  (defn tocmd [cmd]
    (let [parts (string/split (name cmd) #"-")]
      (symbol (apply str "." (first parts)
                     (map string/capitalize (rest parts))))))

  (defn call-form [g cmd args]
    (apply list (tocmd cmd) g args))

  (defn cmd-body [form]
    (let [single? (symbol? form)
          cmd (if single? form (first form))
          args (if single? [] (rest form))
          record-name (gensym (name cmd))
          graphics (gensym)]
      `(do
         (defrecord ~record-name [~@args]
           HumanReadable
           (inspect [_#]
             ;; Adding in "g" as placeholder for passed graphics.
             ~(str (apply str "(" (name (tocmd cmd)) " g " (interpose " " args))
                   ")"))
           Invocable
           (invoke [_# ~graphics]
             ~(apply list (tocmd cmd) graphics args)))
         ~(if single?
            `(def ~cmd (~(symbol (str record-name "."))))
            `(defn ~cmd [~@args]
               (~(symbol (str record-name ".")) ~@args))))))

  (defmacro defcmds [& forms]
    `(do
       ~@(map cmd-body forms))))

(defcmds
  push-matrix
  pop-matrix
  (apply-matrix a b c d e f)

  push-style
  pop-style
  (update-stroke-width w)
  (stroke r g b a)
  (fill r g b a)

  (line x1 y1 x2 y2)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol QuilRenderable
  (compile* [this]))

(declare walk-compile)

(def compile*-seq-method
  `(compile* [this#]
             {:draw (mapcat walk-compile this#)}))

#?(:clj
   (defmacro add-seq-compilers [types]
     `(extend-protocol QuilRenderable
        ~@(interleave types (repeat compile*-seq-method)))))

(add-seq-compilers
   #?(:cljs [List
            LazySeq
            PersistentVector
            IndexedSeq
            ArrayList]
     :clj [clojure.lang.PersistentVector
           clojure.lang.PersistentList
           clojure.lang.ArraySeq
           clojure.lang.IndexedSeq
           clojure.lang.LazySeq]))

(extend-protocol QuilRenderable
  nil
  (compile* [_]
    (println "Can't render nil.")
    [])

  #?(:clj Object :cljs default)
  (compile* [this]
    (if (core/template? this)
      (compile* (core/expand-template this))
      (do
        (println (str "I don't know how to render a " (type this)
                      ". Doing nothing."))
       [])))

  AffineTransformation
  (compile* [{{[a b c d] :matrix [e f] :translation} :atx base :base-shape}]
    (let [mag (util/magnitude a b c d)]
      {:pre [push-matrix
             #_push-style
             #_(line-width mag)
             (apply-matrix a b e c d f)]
       :recur-on base
       :post [pop-matrix
              #_pop-style]}))

  Line
  (compile* [{[x1 y1] :from [x2 y2] :to style :style}]
    {:style style
     :draw [(line x1 y1 x2 y2)]}))

(defn walk-compile [shape]
  (let [c (compile* shape)]
    (concat
     (:pre c)
     (:draw c)
     (when (:recur-on c)
       (walk-compile (:recur-on c)))
     (:post c))))

(defonce t (atom nil))

(defn renderer
  "Returns a render function which when passed a context, renders the given
  shape."
  [graphics shape]
  (q/clear)
  (q/reset-matrix)
  (q/background 200)
  (run! #(invoke % graphics) (walk-compile shape))

  ;; (q/push-matrix)
  ;; (q/apply-matrix 300.0 0.0 0.0 0 300 0)
  ;; (q/apply-matrix 1 0.5 0 0.2 -1.0 0.0)
  ;; (q/line 0 0 1 1)
  ;; (q/pop-matrix)
  )

(defn debug [shape]
  (map inspect (walk-compile shape)))

(defn balanced-compile? [shape]
  (let [inst (walk-compile shape)]
    (= (count (filter #(= push-matrix %) inst))
       (count (filter #(= pop-matrix %) inst)))))

(require '[clojure.pprint :refer [pp pprint]])
