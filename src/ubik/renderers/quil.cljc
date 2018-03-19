(ns ubik.renderers.quil
  (:require [quil.core :as q]
            [ubik.core :as core]
            [ubik.util :refer [import-ubik-types]])
  (:import [ubik.core AffineTransformation Line]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Quil Wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HumanReadable
  (inspect [this]))

(defprotocol Invocable
  (invoke [this]))

(defrecord PushMatrix []
  HumanReadable
  (inspect [_]
    "pushMatrix()")

  Invocable
  (invoke [_]
    (q/push-matrix)))

(def push-matrix (PushMatrix.))

(defrecord PopMatrix []
  HumanReadable
  (inspect [_]
    "popMatrix()")

  Invocable
  (invoke [_]
    (q/pop-matrix)))

(def pop-matrix (PopMatrix.))

(defrecord ApplyMatrix [a b c d e f]
  HumanReadable
  (inspect [_]
    (str
     (apply str "applyMatrix(" (interpose ", " [a b e c d f])) ")"))

  Invocable
  (invoke [_]
    (q/apply-matrix a b e c d f)))

(defn apply-matrix [{[a b c d] :matrix [e f] :translation}]
  (ApplyMatrix. a b c d e f))

(defrecord QLine [x1 y1 x2 y2]
  HumanReadable
  (inspect [_]
    (str "line(" x1 ", " y1 ", " x2 ", " y2 ")"))

  Invocable
  (invoke [_]
    (q/line x1 y1 x2 y2)))

(defn line [[x1 y1] [x2 y2]]
  (QLine. x1 y1 x2 y2))

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
  (compile* [{[a b c d :as atx] :atx base :base-shape}]
    (let [mag (util/magnitude a b c d)]
      {:pre [push-matrix
             push-style
             (line-width mag)
             (apply-matrix atx)]
       :recur-on base
       :post [pop-matrix
              pop-style]}))

  Line
  (compile* [{:keys [from to style]}]
    {:style style
     :draw [(line from to)]}))

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
  (q/stroke-weight (/ 1 300))
  (run! invoke (walk-compile shape))

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
