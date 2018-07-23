(ns ubik.interactive.db (:require [ubik.interactive.impl ]))

(def ^:private app ::app-db)
(def ^:private undo ::undo-graph)
(def ^:private temp ::temp-state)

(defonce ^:private internal-db
  (atom {app  ::uninitialised
         undo {:queue []
               :index -1}
         temp {}}))

(defn get-current-value []
  (get @internal-db app))

(defn- fresh? []
  (= ::uninitialised (get-current-value)))

(defn reset-db! [val]
  (when-not (identical? (get-current-value) val)
    (swap! internal-db assoc app val)))

(defn set-once! [val]
  (when (fresh?)
    (reset-db! val)))

(def the-world (atom nil))

(deftype DBSig []
  ubik.interactive.impl/Subscription
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_]
    (get-current-value)))

(def db-sig (DBSig.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal machinery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- swap-db-raw! [f]
  (swap! internal-db f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:dynamic true
       :private true}
  *max-undo* 50)

(defn- enforce-queue-max [queue index]
  (if (<= (count queue) (inc *max-undo*))
    {:index index
     :queue queue}
    {:index (dec index)
     :queue (into [] (rest queue))}))

(defn- prefix
  [q i]
  (if (= i (count q))
    q
    (into [] (take i q))))

(defn- push-current-snap [db save-fn]
  (let [{:keys [queue index]} (get db undo)
        snapshot     (save-fn (get db app))
        i++          (inc index)]
    (enforce-queue-max (conj (prefix queue i++) snapshot) i++)))

(defn- checkpoint* [save-fn]
  (fn [db]
    (assoc db undo (push-current-snap db save-fn))))

(defn- undo* [save-fn restore-fn]
  (fn [db]
    (let [app-db                (get db app)
          {:keys [index queue]} (get db undo)]
      (if (< 0 index)
        (let [prev (nth queue index)]
          (if (= prev (save-fn app-db))
            (let [i--  (dec index)
                  prev (nth queue i--)]
              (-> db
                  (update undo assoc :index i--)
                  (assoc app (restore-fn app-db prev))))
            (-> db
                (assoc undo (push-current-snap db save-fn))
                (update app restore-fn prev))))
        db))))

(defn- redo* [restore-fn]
  (fn [db]
    (let [{:keys [queue index]} (get db undo)
          i++                   (inc index)]
      (if (< i++ (count queue))
        (let [next (nth queue i++)]
          (-> db
              (assoc app (restore-fn (get db app) next))
              (update undo assoc :index i++)))
        db))))

(defn- swapper
  [max-undo f]
  (fn []
    (binding [*max-undo* max-undo]
      (swap-db-raw! f))))

(defn undo-plugin
  "Returns a "
  [& [{:keys [events save-fn restore-fn max-undo]
       :or   {max-undo   50
              save-fn    identity
              restore-fn (fn [db snapshot] snapshot)
              events     {:undo       :ubik.interactive.core/undo
                          :redo       :ubik.interactive.core/redo
                          :checkpoint :ubik.interactive.core/checkpoint}}}]]
  {:effects {(:undo events)       (swapper max-undo (undo* save-fn restore-fn))
             (:redo events)       (swapper max-undo (redo* restore-fn))
             (:checkpoint events) (swapper max-undo (checkpoint* save-fn))}})
