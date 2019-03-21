(ns ubik.codebase.core
  (:require [ubik.codebase.storage :as store]))

(def master-uri
  "Temp uri of master branch"
  "master.db")

(def snippet-db-uri
  "Just a file at the moment."
  "residential.db")

(defonce
  ^{:dynamic true
    :doc "Current branch. Not that branching is supported robustly at present."}
  *branch*
  (store/branch master-uri))

(def ^:dynamic *store*
  "Default code storage backend."
  (store/file-backed-mem-store snippet-db-uri))
