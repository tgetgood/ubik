(ns ubik.interactive.caching)

(defn cached-fn
  "Returns a memoised version of f, using the global caching strategy."
  [f]
  ;; FIXME: LRU probably, limited size definitely.,
  (memoize f))
