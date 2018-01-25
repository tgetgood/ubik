(ns lemonade.state)

;; Intentionally not defonce so that I can catch attempts to access it before
;; initalisation. The app-db passed into initialise! should be defonced, so hot
;; loading should be okay.
(def internal-db (atom nil))

(defn handle-mutation
  "Like swap!, but signals global state change."
  [[f & args]]
  (apply swap! @internal-db f args))

(defn world
  "Returns the 'world', the root of the render tree. Note, that updates to the
  world tree are batched on animation frames, so this value is only up to date
  as of the last render."
  []
  (:lemonade.core/world @@internal-db))
