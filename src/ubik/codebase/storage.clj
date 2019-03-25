(ns ubik.codebase.storage
  (:refer-clojure :exclude [intern])
  (:require [clojure.java.io :as io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn now []
  (java.util.Date.))

(defn before?
  "Returns true iff t1 is before t2, as per java.util.Date/before."
  [t1 t2]
  (.before t1 t2))

(defn append-line
  "Appends (str x \"\\n\") to file."
  [filename x]
  (spit filename (str x "\n") :append true))

(defn update-ns-map [acc {:keys [op ns/symbol] :as entry}]
  (if (= :add op)
    (assoc-in acc [(namespace symbol) (name symbol)] entry)
    (update acc (namespace symbol) dissoc (name symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol AppendOnlyLog
  (log [this] "Returns the entire list of transactions"))

(defprotocol TimeTravel
  (as-of [this inst] "Returns the list of transactions before inst"))

(defprotocol Store
  (intern [this snippet] "Intern a snippet in the code store")
  (lookup [this id] "Retrieves a code snippet by id."))

(defprotocol ReverseLookup
   (by-value [this snip]
     "Returns the snip if it is in the store, nil otherwise."))

(defprotocol ValueMap
   (as-map [this] "Retrieves the entire store as a map from ids to snippets."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Persistence (snippets)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord FileStore [filename]
  ;; This will fall over very easily. We need indicies. Hell, we need a database.
  Store
  (intern [_ snippet]
    (assert (contains? snippet :id)
            "You're trying to intern a code fragment without an id. You may as
            well drop it on the floor to its face.")
    (let [snippet (assoc snippet :time (now))]
      (append-line filename snippet)
      snippet))
  (lookup [_ id]
    (with-open [rdr (io/reader filename)]
      (->> rdr
           line-seq
           (map read-string)
           (filter #(= (:id %) id))
           first)))

  ReverseLookup
  (by-value [_ snip]
    (let [snip (select-keys snip [:form :links])]
      (with-open [rdr (io/reader filename)]
        (->> rdr
             line-seq
             (map read-string)
             (filter #(= snip (select-keys % [:links :form])))
             first))))

  ValueMap
  (as-map [_]
    (with-open [rdr (io/reader filename)]
      (into {}
            (comp (map read-string)
                  (map (fn [x] [(:id x) x])))
            (line-seq rdr)))))

(defn file-backed-mem-store [filename]
  (let [store (FileStore. filename)
        cache (atom (as-map store))]
    (reify Store
      (intern [_ snippet]
        (let [snippet (intern store snippet)]
          (swap! cache assoc (:id snippet) snippet)
          snippet))
      (lookup [_ id]
        (get @cache id))

      ReverseLookup
      (by-value [_ snip]
        ;; TODO: Indicies
        (let [snip (select-keys snip [:form :links])]
          (first (filter #(= snip (select-keys % [:form :links])) (vals @cache)))))

      ValueMap
      (as-map [_]
        @cache))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord MemLog [history]
  Store
  (intern [_ entry]
    (MemLog. (conj history entry)))
  (lookup [_ sym]
    (let [candidate (last (filter #(= sym (get % :ns/symbol)) history))]
      (when (= :add (:op candidate))
        candidate)))

  TimeTravel
  (as-of [_ inst]
    (MemLog. (into [] (filter #(before? (:time %) inst)) history)))

  ValueMap
  (as-map [_]
    (reduce update-ns-map {} history)))

(defrecord FileBackedBranch [filename]
  Store
  (intern [this entry]
    (let [new (assoc entry :op :add :time (now))]
      (when-let [old (lookup this entry)]
        (append-line filename (assoc old :op :retract)))
      (append-line filename new)
      new))

  (lookup [this sym]
    (with-open [rdr (io/reader filename)]
      (let [hist (->> rdr
                      line-seq
                      (map read-string)
                      (filter #(= sym (get % :ns/symbol)))
                      last)]
        (when (= :add (:op hist))
          hist))))

  AppendOnlyLog
  (log [_]
    (MemLog.
     (with-open [rdr (io/reader filename)]
       (into []
             (map read-string)
             (line-seq rdr)))))

  TimeTravel
  (as-of [this inst]
    (as-of (log this) inst))

  ValueMap
  (as-map [this]
    (with-open [rdr (io/reader filename)]
      (reduce (fn [nses s]
                (let [m   (read-string s)
                      op  (:op m)
                      ns  (namespace (:ns/symbol m))
                      sym (name (:ns/symbol m))]
                  (if (= op :add)
                    (assoc-in nses [ns sym] m)
                    (update nses ns dissoc sym))))
              {}
              (line-seq rdr) ))))

(defn cached-branch [init]
  (let [history (ref init)
        cache   (ref (as-map @history))]
    (reify
      Store
      (intern [this entry]
        (dosync
          (alter history intern entry)
          (alter cache update-ns-map entry)))
      (lookup [this sym]
        (get-in @cache [(namespace sym) (name sym)]))

      TimeTravel
      (as-of [this inst]
        (cached-branch (as-of @history inst)))

      AppendOnlyLog
      (log [_]
        @history)

      ReverseLookup
      (by-value [this entry]
        (let [candidate (lookup this (:ns/symbol entry))]
          (when (apply = (map #(select-keys % [:ns/symbol :id :op])
                              [candidate entry]))
            candidate)))

      ValueMap
      (as-map [_]
        @cache))))

(defn file-cached-branch [filename]
  (let [store (FileBackedBranch. filename)
        cache (cached-branch (log store))]
    (reify
      Store
      (intern [this entry]
        ;; Don't add new entries that only differ by time
        (if-let [candidate (by-value cache entry)]
          candidate
          (locking this
            (let [entry (intern store entry)]
              (intern cache entry)))))
      (lookup [this sym]
        (lookup cache sym))

      TimeTravel
      (as-of [_ inst]
        (as-of cache inst))

      AppendOnlyLog
      (log [_]
        (log cache))

      ValueMap
      (as-map [_]
        (as-map cache)))))

(defn branch [filename]
  (file-cached-branch filename))

(defn branch-lookup
  "Retrieves sym from branch as of time."
  [branch sym time]
  (lookup (as-of branch time) sym))
