(ns lemonade.hosts
  #?(:cljs (:require [lemonade.hosts.browser-canvas :as browser-canvas]
                     [lemonade.system :as system])
     :clj (:require [lemonade.hosts.jvm-quil :as jvm-quil]
                    [lemonade.system :as system])))

#?(:cljs
   (defn html-canvas []
     "Runs lemonade in a the given <canvas> element in the browser."
     (browser-canvas/host (browser-canvas/canvas-elem))))

#?(:clj
   (defn jvm-quil []
     (jvm-quil/host)))

(def ^:dynamic default-host
  #?(:cljs (html-canvas)
     :clj (jvm-quil)))

(def dummy-host
  (reify system/Host
    (event-system [_] nil)
    (render-fn [_] nil)
    (width [_] nil)
    (height [_] nilj)
    (resize-frame [_ _] nil)
    (fullscreen [_] nil)))
