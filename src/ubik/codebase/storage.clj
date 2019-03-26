(ns ubik.codebase.storage
  (:refer-clojure :exclude [intern])
  (:import org.apache.commons.codec.digest.DigestUtils)
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

(defn update-ns-map [acc entry]
  (let [n (:name entry)
        ens (namespace n)
        ename (name n)
        op (:op entry)]
    (if (= :add op)
      (assoc-in acc [ens ename] entry)
      (update acc ens dissoc ename))))

(defn sha1 [x]
  (DigestUtils/sha1Hex (str x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol AppendOnlyLog
  ;; REVIEW: Is retraction an inherent part of append only storage?
  ;; It is required for monotonicity...
  (log [this] "Returns the entire list of transactions")
  (retract [this entry] "Retracts this entry from the log."))

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
  (intern [this snippet]
    (if-let [res (lookup this (sha1 snippet))]
      res
      (let [meta {:time (now) :sha1 (sha1 snippet)}
            entry (assoc meta :snippet snippet)]
        (append-line filename entry)
        (with-meta snippet meta))))
  (lookup [_ id]
    (with-open [rdr (io/reader filename)]
      (let [entry (->> rdr
                       line-seq
                       (map read-string)
                       (filter #(= (:sha1 %) id))
                       first)]
        (when entry
          (with-meta (:snippet entry) (dissoc entry :snippet))))))

  ReverseLookup
  (by-value [this snip]
    (lookup this (sha1 snip)))

  ValueMap
  (as-map [_]
    (with-open [rdr (io/reader filename)]
      (into {}
            (comp (map read-string)
                  (map (fn [x] [(:sha1 x) (with-meta (:snippet x)
                                            (dissoc x :snippet))])))
            (line-seq rdr)))))

(defn file-backed-mem-store [filename]
  (let [store (FileStore. filename)
        cache (atom (as-map store))]
    (reify Store
      (intern [_ snippet]
        (let [hash (sha1 snippet)]
          (if-let [entry (get @cache hash)]
            entry
            (let [snippet (intern store snippet)]
              (swap! cache assoc (:sha1 (meta snippet)) snippet)
              snippet))))
      (lookup [_ id]
        (get @cache id))

      ReverseLookup
      (by-value [this snip]
        (let [hash (if-let [sha (:sha1 (meta snip))] sha (sha1 snip))]
          (lookup this hash)))

      ValueMap
      (as-map [_]
        @cache))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ns-sym [sym ref]
  {:name sym :ref ref})

(defn fill-branch-entry
  "Adds default metadata to branch entry.
  Currently this is just :time and [:op :add]"
  [entry]
  (assoc entry :time (now) :op :add))

(defrecord MemLog [history]
  Store
  (intern [_ entry]
    (MemLog. (conj history (fill-branch-entry entry))))
  (lookup [_ sym]
    (let [candidate (last (filter #(= sym (get % :name)) history))]
      (when (= :add (:op candidate))
        candidate)))

  AppendOnlyLog
  (log [_]
    history)
  (retract [_ entry]

    (let [entry (-> entry
                    fill-branch-entry
                    (assoc :op :retract))]
      (MemLog. (conj history entry))))

  TimeTravel
  (as-of [_ inst]
    (MemLog. (into [] (filter #(before? (:time %) inst)) history)))

  ValueMap
  (as-map [_]
    (reduce update-ns-map {} history)))

(defrecord FileBackedBranch [filename]
  Store
  (intern [this entry]
    (let [new (fill-branch-entry entry)]
      (when-let [old (lookup this (:name entry))]
        (append-line filename (assoc (fill-branch-entry old) :op :retract)))
      (append-line filename new)
      new))

  (lookup [this sym]
    (with-open [rdr (io/reader filename)]
      (let [candidate (->> rdr
                           line-seq
                           (map read-string)
                           (filter #(= sym (get % :name)))
                           last)]
        (when (= :add (:op candidate))
          candidate))))

  AppendOnlyLog
  (log [_]
    (MemLog.
     (with-open [rdr (io/reader filename)]
       (into []
             (map read-string)
             (line-seq rdr)))))
  (retract [_ entry]
    (append-line filename (assoc (fill-branch-entry entry) :op :retract)))

  TimeTravel
  (as-of [this inst]
    (as-of (log this) inst))

  ValueMap
  (as-map [this]
    (with-open [rdr (io/reader filename)]
      (reduce (fn [nses s]
                (let [m   (read-string s)
                      op  (:op m)
                      ns  (namespace (:name m))
                      sym (name (:name m))]
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
         ;; FIXME: Timestamps will be out of sync.
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
      (retract [_ entry]
        (let [entry (assoc (fill-branch-entry entry) :op :retract)]
          (dosync
           (alter history retract entry)
           (alter cache update-ns-map entry))))

      ReverseLookup
      (by-value [this entry]
        (let [candidate (lookup this (:name entry))]
          (when (apply = (map #(select-keys % [:name :ref])
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
      (retract [this entry]
        (locking this
          (retract store entry)
          (retract cache entry)))

      ValueMap
      (as-map [_]
        (as-map cache)))))

(defn branch [filename]
  (file-cached-branch filename))

(defn branch-lookup
  "Retrieves sym from branch as of time."
  [branch sym time]
  (lookup (as-of branch time) sym))
