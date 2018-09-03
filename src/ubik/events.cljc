(ns ubik.events
  (:require #?(:clj [ubik.events.quil :as ev]
               :cljs [ubik.events.browser :as ev])))

(defprotocol EventSystem
  (setup [this host listen dispatch])
  (teardown [this host]))

(defonce default-event-system
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
