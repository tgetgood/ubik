(ns lemonade.db)

;; Intentionally not defonce so that I can catch attempts to access it before
;; initalisation. The app-db passed into initialise! should be defonced, so hot
;; loading should be okay.
(def ^{:dynamic true :private true} *app-db* nil)

(defn- atom? [ref]
  #?(:clj (satisfies? clojure.lang.IAtom ref)
     :cljs (satisfies? IAtom ref)))

(defn _set-db! [ref]
  (assert (atom? ref))
  (set! *app-db* ref))

(defn mutate!
  "Like swap!, but signals global state change."
  [f & args]
  (apply swap! *app-db* f args))

(defn world []
  (:lemonade.core/world @*app-db*))
