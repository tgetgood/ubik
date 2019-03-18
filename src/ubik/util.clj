(ns ubik.util)

(defn vmap
  "Applies f to each value in map m, returning a map with the same keys, and
  transformed values."
  [f m]
  (into {} (map (fn [[k v]] [k (f v)])) m))
