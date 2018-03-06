(ns ubik.hosts
  #?(:cljs (:require [ubik.hosts.browser-canvas :as browser-canvas])
     :clj (:require [ubik.hosts.jvm-quil :as jvm-quil])))

#?(:cljs
   (defn html-canvas
     "Runs ubik in a the given <canvas> element in the browser."
     [opts]
     (browser-canvas/host opts)))

#?(:clj
   (defn quil [opts]
     (jvm-quil/host opts)))

(defn default-host [opts]
  (#?(:clj quil :cljs html-canvas) opts))

;; HACK: I don't want users to have to worry about hosts at all until they need
;; to. Is there a better way to accomplish that?
(set! ubik.core/*host* (default-host {}))
