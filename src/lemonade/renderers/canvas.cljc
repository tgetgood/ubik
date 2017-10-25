(ns lemonade.renderers.canvas
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [lemonade.core :as core]
            [lemonade.spec :as ls]))

;; REVIEW: How much can we do at compile time?

(defn apply-atx [{[a b c d] :matrix [e f] :translation}]
  (fn [ctx]
    (.transform ctx a c b d e f)))

(defmulti process-atx first)

(defmethod process-atx :single
  [[_ atx]]
  [(apply-atx atx) (apply-atx (core/invert-atx atx))])

(defmethod process-atx :composition
  [[_ atxs]]
  (let [fs (map process-atx atxs)]
    ;; TODO: These can be composed at compile time (if known)
    [(apply juxt (map first fs))
     (apply juxt (reverse (map second fs)))]))

(defmulti render-shape :type)

(defmulti render-fn first)

(defmethod render-fn :transformed
  [[_ {:keys [base-shape atx]}]]
  (let [[tx itx]   (process-atx atx)
        sub-render (render-fn base-shape)]
    (fn [ctx]
      (doto ctx
        tx
        sub-render
        itx))))

(defmethod render-fn :primitive
  [[_ shape]]
  (render-fn shape))

(defmethod render-fn :path
  [[_ shape]]
  (let [cont (render-fn shape)]
    (fn [ctx]
      (.beginPath ctx)
      (cont ctx)
      (.stroke ctx))))

(defmethod render-fn :joined-segments
  [[_ {:keys [segments]}]]
  (let [seg-fns (map render-shape segments)]
    (apply juxt seg-fns)))

(defmethod render-fn :single-segment
  [[_ seg]]
  (render-shape seg))

(defmethod render-fn :builtin
  [[_ shape]]
  (println "builtin"))

(defmethod render-shape ::ls/line
  [{:keys [from to style]}]
  (fn [ctx]
    (.moveTo ctx (first from) (second from))
    (.lineTo ctx (first to) (second to))))

(defmethod render-shape ::ls/bezier
  [{[x1 y1] :from [x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2}]
  (fn [ctx]
    (.moveTo ctx x1 y1)
    (.bezierCurveTo ctx cx1 cy1 cx2 cy2 x2 y2)))

(defn renderer [shape]
  (render-fn (s/conform ::ls/shape shape)))
