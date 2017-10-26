(ns lemonade.renderers.canvas
  (:require [#?(:cljs cljs.pprint :clj clojure.pprint) :refer [pprint]]
            [clojure.spec.alpha :as s]
            [lemonade.core :as core]
            [lemonade.geometry :as geometry]))

(defmulti render-shape :type)

(defmulti render-fn first)

(defn renderer [shape]
  (let [tree (s/conform ::core/shape shape)]
    (if (= :cljs.spec.alpha/invalid tree)
      (do
        (println "Cannot parse shape:")
        (pprint shape))
      (render-fn tree))))

;; REVIEW: How much can we do at compile time?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal render logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-atx [{[a b c d] :matrix [e f] :translation}]
  (fn [ctx]
    (.transform ctx a c b d e f)))

(defmethod render-fn :transformed
  [[_ {:keys [base-shape atx]}]]
  (let [tx   (apply-atx atx)
        itx  (apply-atx (geometry/invert-atx atx))
        cont (render-fn base-shape)]
    (fn [ctx]
      (doto ctx
        tx
        cont
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
  (if (empty? segments)
    (constantly nil)
    (let [seg-fns (map render-shape segments)
          cont (apply juxt seg-fns)]
      (fn [ctx]
        (cont ctx)
        (when (geometry/closed? segments)
          (.closePath ctx))))))

(defmethod render-fn :single-segment
  [[_ seg]]
  (render-shape seg))

(defmethod render-fn :template
  [[_ shape]]
  (renderer (core/template-expand shape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Leaf renderers
;;
;; At some point we have to render something, Less and less though, it would
;; appear.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn angle [[t a]]
  (geometry/parse-angle a))

(defmethod render-shape ::core/arc
  [{[x y] :centre r :radius :keys [from to style]}]
  (let [from (angle from)
        to (angle to)]
    (println to)
    (fn [ctx]
      (.moveTo ctx (+ x r) y)
      (.arc ctx x y r from to))))

(defmethod render-shape ::core/line
  [{:keys [from to style]}]
  (fn [ctx]
    (.moveTo ctx (first from) (second from))
    (.lineTo ctx (first to) (second to))))

(defmethod render-shape ::core/bezier
  [{[x1 y1] :from [x2 y2] :to [cx1 cy1] :c1 [cx2 cy2] :c2}]
  (fn [ctx]
    (.moveTo ctx x1 y1)
    (.bezierCurveTo ctx cx1 cy1 cx2 cy2 x2 y2)))
