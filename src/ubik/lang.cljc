(ns ubik.lang
  (:refer-clojure :exclude [+ - * vector Vector IVector])
  (:require [#?(:clj clojure.core :cljs cljs.core) :as cc]
            [ubik.math :as math]))

(defn error [m]
  (throw (#?(:clj Exception. :cljs js/Error.) m)))

(defprotocol Shape
  (origin [this] "The point preserved under linear transforms in this shape."))

(defprotocol Vectorial
  (base-vector [this]))

(extend-protocol Vectorial
  #?(:clj Object :cljs default)
  (base-vector [_] nil))

(defn vector? [x]
  (boolean (base-vector x)))

(defprotocol IVector
 (unit [this])
 (dot [this o])
 (length [this]))

(defprotocol LinearAlgebra
  (dimension [this])
  (v+ [this o])
  (neg [this])
  (v- [this o])
  (left-mult [this o]))

(defn vectorise [x]
  (if-let [v (base-vector x)]
    v
    (error (str x " has not vector nature"))))

(extend-type #?(:clj clojure.lang.IPersistentVector
                :cljs cljs.core/PersistentVector)
  IVector
  (unit [this]
    (let [l (length this)]
      (mapv #(/ % l) this)))
  (length [this]
    (math/sqrt (reduce cc/+ (map #(cc/* % %) this))))
  (dot [this o]
    (reduce cc/+ (map cc/* this (vectorise o))))

  LinearAlgebra
  ;; TODO: Make sure dimensions are always equal
  (dimension [this] [1 (count this)])
  (v+ [this o]
    (mapv cc/+ this (vectorise o)))
  (neg [this]
    (mapv cc/- this))
  (v- [this o]
    (v+ this (neg (vectorise o))))
  (left-mult [this s]
    (assert (number? s))
    (mapv #(cc/* s %) this))

  Vectorial
  (base-vector [this]
    (when (every? number? this)
      this)))

(defn +
  ([] 0)
  ([x] x)
  ([x y]
   (cond
     (number? x) (cc/+ x y)
     (vector? x) (v+ (vectorise x) y)
     :else nil))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn -
  ([x]
   (cond
     (vector? x) (neg (vectorise x))
     :else (cc/- x)))
  ([x y]
   (cond
     (number? x) (cc/- x y)
     (vector? x) (v+ (vectorise x) (neg (vectorise y)))
     :else nil))

  ([x y & more]
   (reduce - (- x y) more)))

(defn *
  ([] 1)
  ([x] x)
  ([x y]
   (cond
     (number? y) (cc/* x y)
     (vector? y) (left-mult (vectorise y) x)
     :else nil))
  ([x y & more]
   (reduce * (* x y) more)))
