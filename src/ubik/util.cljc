(ns ubik.util)

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
