(ns ubik.hosts
  #?(:cljs (:require [ubik.hosts.browser-canvas :as browser-canvas])
     :clj (:require [ubik.hosts.jvm-quil :as jvm-quil])))

#?(:cljs
   (defn html-canvas
     "Runs ubik in a the given <canvas> element in the browser."
     [opts]
     (browser-canvas/host opts)))

#?(:clj
   (defn quil
     "Creates a Processing applet and prepares to run Ubik in it."
     [opts]
     (jvm-quil/host opts)))

(defn default-host [opts]
  (#?(:clj quil :cljs html-canvas) opts))
