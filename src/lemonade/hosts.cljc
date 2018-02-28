(ns lemonade.hosts
  #?(:cljs (:require [lemonade.hosts.browser-canvas :as browser-canvas])
     :clj (:require [lemonade.hosts.jvm-quil :as jvm-quil])))

#?(:cljs
   (defn html-canvas [opts]
     "Runs lemonade in a the given <canvas> element in the browser."
     (browser-canvas/host opts)))

#?(:clj
   (defn jvm-quil [opts]
     (jvm-quil/host opts)))

(def ^:dynamic default-host
  #?(:cljs (html-canvas {})
     :clj (jvm-quil {})))
