(ns ubik.cache)

(defn cached-fn
  "Returns a memoised version of f using the standard caching strategy."
  [f]
  (memoize f))
