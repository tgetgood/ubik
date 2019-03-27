(ns ubik.codebase.config
  (:require [ubik.codebase.storage :as store]))

(def master-uri
  "Temp uri of master branch"
  "master.db")

(def snippet-db-uri
  "Just a file at the moment."
  "residential.db")

(def ^:dynamic *branch*
  "Current branch. Not that branching is supported robustly at present."
  (store/branch master-uri))

(def ^:dynamic *code*
  "Default code storage backend."
  (store/file-backed-mem-store snippet-db-uri))
