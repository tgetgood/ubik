(ns lemonade.draw
  "Simple drawing and animation functions. Like system but far simpler to use."
  (:require [lemonade.hosts :as hosts]
            [lemonade.system :as system]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Simple Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw!
  ([shape] (draw! shape hosts/default-host))
  ([shape host] ((system/render-fn host) shape)))
