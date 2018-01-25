(ns lemonade.draw
  "Simple drawing and animation functions. Like system but far simpler to use."
  (:require [lemonade.hosts :as hosts]
            [lemonade.hosts.protocol :refer [render-fn]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Simple Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw!
  ([shape] (draw! shape hosts/default-host))
  ([shape host] ((render-fn host) shape)))
