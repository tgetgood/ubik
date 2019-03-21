(ns ubik.codebase.storage
  (:refer-clojure :exclude [intern])
  (:require [clojure.java.io :as io]
            [ubik.rt :as rt]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn now []
  (java.util.Date.))

(defn append-line
  "Appends (str x \"\\n\") to file."
  [filename x]
  (spit filename (str x "\n") :append true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Store
  (intern [this snippet] "Intern a snippet in the code store")
  (lookup [this id] "Retrieves a code snippet by id."))

(defprotocol ReverseLookup
   (by-value [this snip]
     "Returns the snip if it is in the store, nil otherwise."))

(defprotocol ValueMap
   (as-map [this] "Retrieves the entire store as a map from ids to snippets."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord FileBackedBranch [filename]
  Store
  (intern [this entry]
    (let [new (assoc entry :op :add :time (now))]
      (when-let [old (lookup this entry)]
        (append-line filename (assoc old :op :retract)))
      (append-line filename new )))
  (lookup [this sym]
    (with-open [rdr (io/reader filename)]
      (->> rdr
           line-seq
           (map read-string)
           (filter #(= sym (get % :ns/symbol)))
           first)))

  ValueMap
  (as-map [this]
    (with-open [rdr (io/reader filename)]
      (reduce (fn [nses s]
                (let [m (read-string s)
                      op (:op m)
                      ns (namespace (:ns/symbol m))
                      sym (name (:ns/symbol m))]
                  (if (= op :add)
                    (assoc-in nses [ns sym] m)
                    (update nses ns dissoc sym))))
              {}
              (line-seq rdr) ))))

(defn branch [filename]
  (FileBackedBranch. filename))

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
    (let [snip (dissoc snip :id :time)]
      (with-open [rdr (io/reader filename)]
        (->> rdr
             line-seq
             (map read-string)
             (filter #(= snip (dissoc % :id :time)))
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
          (swap! cache assoc (:id snippet) snippet)))
      (lookup [_ id]
        (get @cache id))

      ReverseLookup
      (by-value [_ snip]
        ;; TODO: Indicies
        (let [snip (dissoc snip :id :time)]
          (first (filter #(= snip (dissoc % :id :time)) (vals @cache)))))

      ValueMap
      (as-map [_]
        @cache))))
