(ns lemonade.renderers.util
  (:require [lemonade.core :as core]
            [lemonade.math :as math]))

(def noop
  "What a render-fn returns if it wants to do nothing."
  (constantly nil))

(defn render-catchall [shape]
  (if (nil? shape)
    (println "I don't know how to render nil.")
    (println (str "I don't know how to render a " (type shape))))
  noop)

(defn magnitude [a b c d]
  ;; HACK: This works for symmetric linear transforms, but as soon as we start
  ;; talking about skews and asymmetric scalings, it breaks down. I don't see
  ;; any way to manage this without writing my own pixel shaders. Hopefully I do
  ;; before I do.
  ;; !!!!!!!
  ;; Unless I ditch paths altogether and use bezier curves --- actually pairs of
  ;; curves --- to represent the edges of objects. They have no stroke, just a
  ;; fill, and so I can control exactly how thick the line is at all points. Soo
  ;; much work... But it has the potential to be a solution.
  (let [m (math/sqrt (math/det a b c d))]
    (if (math/nan? m)
      1
      m)))
