(ns ubik.interactive.transducers)

(defn emit
  ([v]
   (fn [_ rf acc]
     (rf acc v)))
  ([v & args]
   (fn [_ rf acc]
     (let [v' (rf acc v)]
       (if (reduced? v')
         v'
         (reduce rf v' args))))))

(defn emit-state
  ([s]
   (fn [sv rf acc]
     (vreset! sv s)
     acc))
  ([s v]
   (fn [sv rf acc]
     (vreset! sv s)
     (rf acc v)))
  ([s v & args]
   (fn [sv rf acc]
     (vreset! sv s)
     (let [v' (rf acc v)]
       (if (reduced? v')
         v'
         (reduce rf v' args))))))

(defn transducer
  ([next-fn]
   (fn [rf]
     (fn
       ([] (rf))
       ([acc] (rf acc))
       ([acc x]
        (let [emission (next-fn x)]
          (if (fn? emission)
            (emission nil rf acc)
            acc))))))
  ([init-state next-fn]
   (transducer init-state next-fn (constantly nil)))
  ([init-state next-fn flush-fn]
   (fn [rf]
     (let [state (volatile! init-state)]
       (fn
         ([] (rf))
         ([acc]
          (let [emission (flush-fn @state)]
            (if (fn? emission)
              (rf (unreduced (emission state rf acc)))
              (rf acc))))
         ([acc x]
          (let [emission (next-fn @state x)]
            (if (fn? emission)
              (emission state rf acc)
              acc))))))))

(defn map* [f]
  (transducer (fn [x] (emit (f x)))))

(defn filter* [p]
  (transducer (fn [x] (when (p x) (emit x)))))

(defn partition-all* [n]
  (transducer
   []
   (fn [buffer next]
     (let [buffer' (conj buffer next)]
       (if (= (count buffer') n)
         (emit-state [] buffer')
         ;; Poor name choice. We're not emitting anything.
         (emit-state buffer'))))
   (fn [buffer]
     (emit buffer))))

(defn partition-all** [^long n]
  (transducer
   (java.util.ArrayList. n)
   (fn [^java.util.ArrayList buffer next]
     (.add buffer next)
     (when (= n (.size buffer))
              (let [v (vec (.toArray buffer))]
                (.clear buffer)
                (emit v))))
   (fn [^java.util.ArrayList buffer]
     (when-not (.isEmpty buffer)
       (let [v (vec (.toArray buffer))]
         ;;clear first!
         (.clear buffer)
         (emit v))))))

(defn mapcat* [f]
  (transducer (fn [x] (apply emit (f x)))))
