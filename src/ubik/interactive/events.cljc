(ns ubik.interactive.events
  #?(:cljs (:require [ubik.interactive.events.browser :as ev])))

(defn start-event-system! [dispatch-fn]
  #?(:cljs (ev/setup (ev/canvas-elem) dispatch-fn)))
