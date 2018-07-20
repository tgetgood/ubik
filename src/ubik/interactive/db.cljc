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
;;;;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def max-undo 50)

(defn restore-db [db-val snapshot]
  (assoc db-val :shapes snapshot))

(defn save-db [db-val]
  (:shapes db-val))

(defn enforce-queue-max [queue index]
  (if (< (count queue) max-undo)
    {:index index
     :queue queue}
    {:index (dec index)
     :queue (into [] (rest queue))}))

(defn push-current-snap [db save-fn]
  (let [{:keys [queue index]} (get db undo)
        snapshot     (save-fn (get db app))
        i++          (inc index)]
    (if (= i++ (count queue))
      (enforce-queue-max (conj queue snapshot) i++)
      (enforce-queue-max (conj (into [] (take i++ queue)) snapshot) i++))))

(defn checkpoint* [save-fn]
  (swap! internal-db
         (fn [db]
           (assoc db undo (push-current-snap db save-fn)))))

(def checkpoint! (partial checkpoint* save-db))

(defn undo* [save-fn restore-fn]
  (swap! internal-db
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
               db)))))

(def undo! (partial undo* save-db restore-db))

(defn redo* [restore-fn]
  (swap! internal-db
         (fn [db]
           (let [{:keys [queue index]} (get db undo)
                 i++                   (inc index)]
             (if (< i++ (count queue))
               (let [next (nth queue i++)]
                 (-> db
                     (assoc app (restore-fn (get db app) next))
                     (update undo assoc :index i++)))
               db)))))

(def redo! (partial redo* restore-db))
