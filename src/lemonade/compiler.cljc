(ns lemonade.compiler
  (:refer-clojure :exclude [compile])
  (:require [lemonade.core :as core
             #?@(:cljs [:refer [CompiledShape Region Composite
                                AffineTransformation Frame]])])
  #?(:cljs (:require-macros [lemonade.compiler :refer [extend-to-sequentials]])
     :clj (:import [lemonade.core CompiledShape Region Composite
                    AffineTransformation Frame])))

#?(:clj
   (defmacro extend-to-sequentials
     {:style/indent [1 :form :form [1]]}
     [prot types [method [this & args] & body]]
     `(extend-protocol ~prot
        ~@(mapcat
            (fn [t]
              [t `(~method [~this ~@args]
                   (let [~this (into (empty ~this)
                                     (map #(~method % ~@args) ~this))]
                     ~@body))])
            types))))

(def render-compile identity)
(def geom-compile identity)

(defn compile-1 [shape]
  (core/compiled-shape shape (render-compile shape) (geom-compile shape)))

(defprotocol Compilable
  (compile [shape]))

(extend-to-sequentials Compilable
  #?(:cljs [List
            LazySeq
            PersistentVector
            IndexedSeq
            ArrayList]
     :clj [clojure.lang.PersistentVector])
  (compile [shape]
    (compile-1 shape)))

(extend-protocol Compilable
  CompiledShape
  (compile [shape] shape)

  Region
  (compile [shape]
    (compile-1 (update shape :boundary compile)))

  AffineTransformation
  (compile [shape]
    (compile-1 (update shape :base-shape compile)))

  Frame
  (compile [shape]
    (compile-1 (update shape :base-shape compile)))

  Composite
  (compile [shape]
    (compile-1 (update shape :contents compile)))

  default
  (compile [shape]
    (compile-1 shape)))

(require '[clojure.pprint :refer [pprint pp]])
