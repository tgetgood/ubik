(ns ubik.interactive.events
  (:require ubik.core
            #?(:clj [ubik.interactive.events.quil :as ev]
               :cljs [ubik.interactive.events.browser :as ev])
            ubik.hosts))

(defprotocol EventSystem
  (setup [this host listen dispatch])
  (teardown [this host]))

(def default-event-system
  #?(:cljs
     (reify
       EventSystem
       (setup [_ host listen dispatch]
         (ev/connect-events host listen dispatch))
       (teardown [_ host]
         (ev/disconnect-events host)))
     :clj
     (reify
       EventSystem
       (setup [_ _ _ _])
       (teardown [_ _]))))
