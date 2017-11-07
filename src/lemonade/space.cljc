(ns lemonade.space
  #?(:cljs (:require-macros [lemonade.space :refer [distance-fn]]))
  (:require [lemonade.core :as core]
            [lemonade.geometry :as geometry]
            [lemonade.spec.geometry :as gs]
            [clojure.spec.alpha :as s]))

;;; Given a point in space and a shape tree, detect which paths in the tree were
;;; clicked on.

(defn classify [x]
  (if (s/valid? ::gs/point x)
    ::gs/point
    (:type x)))

(defmulti distance (fn [x y] [(classify x) (classify y)]))

#?(:clj
   (defmacro distance-fn
     "Defines a symmetrically polymorphic symmetric function"
     {:style/indent 1}
     [types args & body]
     `(do
        (defmethod distance [~@types] [~@args] ~@body)
        (when-not (= [~@types] [~@(reverse types)])
          (defmethod distance [~@(reverse types)] [~@(reverse args)] ~@body)))))

(distance-fn [::gs/point ::gs/point]
  [[x1 y1] [x2 y2]]
  (geometry/sqrt (+ (geometry/exp (- x2 x1) 2) (geometry/exp (- y2 y1) 2))))

(distance-fn [::gs/point ::core/line]
  [[x y] {[px py] :from [qx qy] :to}]
  (let [pq [(- qx px) (- qy py)]
        t* (- (/ (geometry/dot [(- px x) (- py y)] pq)
                 (geometry/dot pq pq)))
        t (min 1 (max 0 t*))
        s (map + [px py] (map (partial * t) pq))]
    (distance s [x y])))

(distance-fn [::gs/point ::core/circle]
  [p {:keys [radius centre]}]
  (max 0 (- (distance p centre) radius)))

(distance-fn [::gs/point ::core/rectangle]
  [[x y] {[p q] :corner w :width h :height :as rect}]
  (if (and (< p x (+ p w)) (< q y (+ q h)))
    0
    (let [lines (:contents (core/template-expand-all rect))]
      (apply min (map (partial distance [x y]) lines)))))

(defn trace* [shape point]
  (cond
    (sequential? shape)           (mapv #(trace* % point) shape)

    (contains? shape :contents)   (mapv #(trace* % point) (:contents shape))

    (contains? shape :base-shape) (trace* (:base-shape shape) point)

    :else                         (distance point shape)))

(defn trace [shape point]
  (->> (trace* shape point)
       flatten
       sort))
