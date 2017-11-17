(ns lemonade.events.core)

(defonce ^:private registered-callbacks (atom {}))

(defn fire! [evt]
  (loop [[{cb ::cb} & more] (get @registered-callbacks (:lemonade.events/type evt))]
    (when cb
      (cb evt))
    (when more
      (recur more))))

(defn register-event-handlers [hmap key]
  (loop [[[k v] & more] hmap]
    (swap! registered-callbacks update k conj {::cb v ::key key})
    (when more
      (recur more))))

(defn deregister-event-handlers
  "Removes event handler of event type with matching key.
  N.B.: this performs a linear scan of all registered handlers and can be slow."
  ;; REVIEW: There should be a fast way to do this, shouldn't there?
  [type key]
  (swap! registered-callbacks update type (partial remove #(= key (::key %)))))

(defn clear-events!
  "This might be needed for reloading."[]
  (reset! registered-callbacks {}))
