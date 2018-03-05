(ns ubik.interactive.db)

;; Intentionally not defonce so that I can catch attempts to access it before
;; initalisation. The app-db passed into initialise! should be defonced, so hot
;; loading should be okay.
(defonce app-db (atom nil))

(defn handle-mutation
  "Like swap!, but signals global state change."
  [[f & args]]
  (apply swap! @app-db f args))
