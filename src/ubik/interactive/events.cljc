(ns ubik.interactive.events
  #?(:cljs (:require [ubik.interactive.events.browser :as ev]
                     ubik.core
                     ubik.hosts)))

(defprotocol HostEvents
  (event-signal [this] "Returns a signal which takes on all events."))


#?(:cljs
   (extend-type ubik.hosts/HTMLCanvasHost
     HostEvents
     (event-signal [this]
       (ev/event-signal (ubik.core/base this)))))

#?(:clj
   (extend-type clojure.lang.PersistentArrayMap
     HostEvents
     (event-signal [_])))
