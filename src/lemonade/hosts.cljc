(ns lemonade.hosts
  #?(:cljs (:require [lemonade.hosts.browser-canvas :as browser-canvas])
     :clj (:require [lemonade.hosts.jvm-quil :as jvm-quil])))

#?(:cljs
   (defn html-canvas []
     "Runs lemonade in a the given <canvas> element in the browser."
     (browser-canvas/host (browser-canvas/canvas-elem))))

#?(:clj
   (defn jvm-quil []
     (jvm-quil/host)))

(def default-host
  #?(:cljs (html-canvas)
     :clj (jvm-quil)))
