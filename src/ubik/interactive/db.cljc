(ns ubik.interactive.db)

(defonce app-db (atom ::uninitialised))

(def the-world (atom nil))
