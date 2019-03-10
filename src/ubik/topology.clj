(ns ubik.topology
  (:require [clojure.core.async :as async]
            [clojure.string :as string]
            [datomic.api :as d]
            [ubik.codebase :as cb :refer [current-branch]]
            [ubik.db :refer [push pull conn]]))

(defn set-topology! [tops]
  (let [topo-id (push tops)]
    (d/transact conn
                [{:branch/name     (current-branch)
                  :branch/topology topo-id}])))

(defn current-topology []
  (pull (d/q '[:find ?t .
               :in $ ?b
               :where
               [?bid :branch/name ?b]
               [?bid :branch/topology ?t]]
             (d/db conn)
             (current-branch))))

(defn vmap [f m]
  (into {} (map (fn [[k v]] [k (f v)])) m))

(defn listen [x]
  {:out (async/mult x)})

(defn process [x]
  (let [out-ch (async/chan (async/sliding-buffer 128))
        in-map (vmap #(async/chan) x)]
    (doseq [[k ch] in-map]
      (async/go-loop [state {}]
        (when-let [msg (async/<! ch)]
          (let [res (atom state)]
            (try
              (reset! res ((get x k) state msg)
                      (when (contains? res :emit)
                        (async/>! out-ch (:emit res)))
                      (when (contains? res :emit-all)
                        (run! #(async/put! out-ch %) (:emit-all res))))
              (catch Exception e (reset! res state)))
            (recur @res)))))
    {:in in-map :out (async/mult out-ch)}))

(defn l2!
  "We need some sort of subscription interface. Tapping, really, "
  [ch]
  ch)

(defn wire [channels send-map receiver]
  (doseq [[edge sender] send-map]
    (let [send-ch (get-in channels [sender :out])
          rec-ch (get-in channels [receiver :in edge])]
      (async/pipe (async/tap send-ch rec-ch)))))

(defn activate!
  "As a first step, we're just going to start the new topology and let the old
  ones lay around.

  Inputs can be listened to, returning channels, effectors are assumed to be
  receiving channels, and nodes, when processed, become multiplexers which can
  be wired together.

  O! this is so ad hoc."
  [{:keys [inputs effectors nodes edges]}]
  (let [input-chans (vmap listen inputs )
        node-chans (vmap process nodes)
        all (merge node-chans input-chans)]
    (doseq [[receiver send-map] edges]
      (wire all receiver send-map))))

(defn replace-topology! [t]
  (set-topology! t)
  (activate! t))
