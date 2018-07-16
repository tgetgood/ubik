(ns ubik.lang
  (:refer-clojure :exclude [+ - * vector Vector])
  (:require [#?(:clj clojure.core :cljs cljs.core) :as cc]
            [ubik.math :as math]))

(defn error [m]
  (throw (#?(:clj Exception. :cljs js/Error.) m)))

(defprotocol Shape
  (origin [this] "The point preserved under linear transforms in this shape."))

(defprotocol Vectorial
  (base-vector [this]))

(defprotocol IVector
 (unit [this])
 (dot [this o])
 (length [this]))

(defprotocol LinearAlgebra
  (dimension [this])
  (+ [this o])
  (- [this] [this o])
  (* [this o]))

(defn vectorial? [x]
  (satisfies? Vectorial x))

(defn vectorise [x]
  (base-vector x))

(declare vector)

(defrecord Vector [elements]
  #?(:clj clojure.lang.Indexed)
  #?(:clj (nth [this i]
               (nth elements i)))
  #?(:clj (nth [_ i not-found]
               (nth elements i not-found)))

  #?(:cljs IIndexed)
  #?(:cljs (-nth [this i]
                 (nth elements i)))
  #?(:cljs (-nth [this i not-found]
                (nth elements i not-found)))
  Vectorial
  (base-vector [this] this)

  IVector
  (unit [this]
    (let [l (length this)]
      (vector (mapv #(/ % l) elements))))
  (length [_]
    (math/sqrt (reduce cc/+ (map #(cc/* % %) elements))))
  (dot [_ o]
    (reduce cc/+ (map cc/* elements (:elements (vectorise o)))))

  LinearAlgebra
  ;; TODO: Make sure dimensions are always equal
  (dimension [_] (count elements))
  (+ [this o]
    (vector (mapv cc/+ elements (:elements (vectorise o)))))
  (- [_]
    (vector (mapv cc/- elements)))
  (- [this o]
    (+ this (- (vectorise o))))
  (* [_ o] nil))

(defn vector [es]
  (Vector. es))

(extend-type clojure.lang.IPersistentVector
  Vectorial
  (base-vector [this]
    (when (every? number? this)
      (vector this))))

(extend-type #?(:clj Number :cljs js/Number)
  LinearAlgebra
  (dimension [_] 0)
  (* [this v]
    (cond
      (number? v)    (cc/* this v)
      (vectorial? v) (vector (mapv #(cc/* this %) (:elements (vectorise v))))
      :else nil))
  (+ [this v]
    (when (number? v)
      (cc/+ this v)))
  (- [this] (cc/- this))
  (- [this o]
    (when (number? o)
      (cc/- this o))))
