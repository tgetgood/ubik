(ns ubik.interactive.db)

(defonce app-db (atom ::uninitialised))

(defn- fresh? [db-ref]
  (= ::uninitialised @db-ref))

(defn get-current-value []
  @app-db)

(defn reset-db! [val]
  (when-not (identical? @app-db val)
    (reset! app-db val)))

(defn set-once! [val]
  (when (fresh? app-db)
    (reset-db! val)))

(def the-world (atom nil))
