(ns lemonade.renderers.canvas
  (:require [lemonade.core :as core]
            [lemonade.spec :as sl]))

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
  (let [fs (map process-atx atxs)
        ifs (->> atxs (map core/invert-atx) (map process-atx) reverse)]
    ;; TODO: These can be composed at compile time (if known)
    [(juxt fs)
     (juxt ifs)]))

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
  (render-fn shape))

(defmethod render-fn :joined-segments
  [[_ {:keys [segments]}]]
  (let [seg-fns (map render-fn segments)]
    (fn [ctx]
      (.beginPath ctx)
      ((juxt seg-fns)))))

(defmethod render-fn :single-segment
  [[_ seg]]
  (render-shape seg))

(defmethod render-shape ::sl/line
  [{:keys [from to style]}]
  (fn [ctx]
    (.moveTo ctx (first from) (second from))
    (.lineTo ctx (first to) (second to))))
