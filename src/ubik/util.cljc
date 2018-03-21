(ns ubik.util
  (:require [net.cgrand.macrovich :as macros :include-macros true]))

(def ubik-types
  '[Region Composite Frame
    Line Bezier Arc
    Circle Annulus PolyLine Rectangle
    AffineTransformation
    RawText Text])

(defn import-ubik-types
  "Imports core Ubik shape types into the current ns appropriately for either
  clj or cljs. Avoids messy and brittle ns forms."
  ;; REVIEW: This is not idiomatic, nor is it recommended. What are the
  ;; downsides?
  []
  #?(:clj `(import [ubik.core ~@ubik-types])
     :cljs `(require [ubik.core :refer ~ubik-types])))

(macros/deftime
  (defmacro implement-sequentials
    {:style/indent [1 :form [1]]
     :doc "Generates boilerplate to implement a protocol identically for all
     sequential things."}
    [prot & methods]
    (let [types (macros/case :cljs '[List
                                     LazySeq
                                     PersistentVector
                                     IndexedSeq
                                     ChunkedSeq
                                     ArrayList]
                             :clj '[clojure.lang.PersistentVector
                                    clojure.lang.PersistentList
                                    clojure.lang.ArraySeq
                                    clojure.lang.IndexedSeq
                                    clojure.lang.PersistentVector$ChunkedSeq
                                    clojure.lang.LazySeq])]
      `(extend-protocol ~prot
         ~@(mapcat (fn [a b] `[~a ~@b]) types (repeat methods))))))
