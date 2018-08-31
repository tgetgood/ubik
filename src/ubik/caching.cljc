(ns ubik.caching)

(defn cached-fn
  "Returns a memoised version of f, using the global caching strategy."
  [f]
  ;; FIXME: LRU probably, limited size definitely.,
  (memoize f))

(defn cache-1
  "Returns a version of f which caches only the most recent value."
  [f]
  (let [last-arg (volatile! (gensym "NO-MATCH"))
        last-val (volatile! nil)]
    (fn [& args]
      (if (= args @last-arg)
        @last-val
        (let [ret (apply f args)]
          (vreset! last-arg args)
          (vreset! last-val ret)
          ret)))))
