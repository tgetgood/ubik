(ns ubik.interactive.events
  #?(:cljs (:require [ubik.interactive.events.browser :as ev]
                     ubik.core
                     ubik.hosts)))

(defprotocol HostEvents
  (wire-events [this queue])
  (cleanup [this]))


#?(:cljs
   (extend-type ubik.hosts/Host
     HostEvents
     (wire-events [this queue]
       (ev/setup (ubik.core/base this) #(swap! queue conj %)))
     (cleanup [this]
       (ev/teardown this))))

#?(:clj
   (extend-type clojure.lang.PersistentArrayMap
     HostEvents
     (wire-events [_ _])
     (cleanup [_])))
