(ns ubik.hosts
  #?(:cljs (:require [ubik.hosts.browser-canvas :as browser-canvas])
     :clj (:require [ubik.hosts.jvm-quil :as jvm-quil])))

#?(:cljs
   (defn html-canvas [opts]
     "Runs ubik in a the given <canvas> element in the browser."
     (browser-canvas/host opts)))

#?(:clj
   (defn jvm-quil [opts]
     (jvm-quil/host opts)))

(def ^:dynamic default-host
  #?(:cljs (html-canvas {})
     :clj (jvm-quil {})))
