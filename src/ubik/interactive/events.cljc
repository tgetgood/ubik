(ns ubik.interactive.events
  (:require ubik.core
            #?(:clj [ubik.interactive.events.quil :as ev]
               :cljs [ubik.interactive.events.browser :as ev])
            ubik.hosts))

(defprotocol HostEvents
  (wire-events [this queue])
  (cleanup [this]))


(extend-type #?(:clj ubik.hosts.Host :cljs ubik.hosts/Host)
   HostEvents
   (wire-events [this enqueue]
     (ev/setup (ubik.core/base this) enqueue))
   (cleanup [this]
     (ev/teardown this)))

#?(:clj
   (extend-type clojure.lang.PersistentArrayMap
     HostEvents
     (wire-events [_ _])
     (cleanup [_])))
