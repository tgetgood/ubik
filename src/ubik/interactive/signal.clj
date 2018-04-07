(ns ubik.interactive.signal)

(deftype Signal [^:volatile-mutable value _meta ]
  clojure.lang.IDeref
  (deref [_] value)

  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ meta])

  clojure.lang.IReduce
  (reduce [_ f]
    (f (f) value))
  (reduce [_ f start]
    (f start value)))

#_(defn signal [tx input]
  (into (empty (Signal. 3 4)) tx input)
  )
