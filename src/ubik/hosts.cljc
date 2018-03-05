(ns ubik.hosts
  #?(:cljs (:require [ubik.hosts.browser-canvas :as browser-canvas])
     :clj (:require [ubik.hosts.jvm-quil :as jvm-quil])))

#?(:cljs
   (defn html-canvas [opts]
     "Runs ubik in a the given <canvas> element in the browser."
     (browser-canvas/host opts)))

#?(:clj
   (defn quil [opts]
     (jvm-quil/host opts)))

(defn default-host [opts]
  (#?(:clj quil :cljs html-canvas) opts))
