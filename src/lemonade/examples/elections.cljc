(ns lemonade.examples.elections
  "Demo of shape manipulation via affine transformations"
  (:require [lemonade.core :as core
             :refer [scale translate line with-style]
             #?@(:cljs [:include-macros true])]
            [lemonade.geometry :as geometry]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Example Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def election-data
  {:2015
   {:results {"Libertarian"                                             36775
              "No Affiliation"                                          9007
              "United Party"                                            57
              "Communist"                                               4393
              "Seniors Party"                                           157
              "CAP"                                                     401
              "Conservative"                                            5613633
              "Liberal"                                                 6942937
              "Independent"                                             40609
              "Forces et Démocratie - Allier les forces de nos régions" 8274
              "Christian Heritage Party"                                15232
              "PC Party"                                                4476
              "Pirate"                                                  908
              "Animal Alliance/Environment Voters"                      1699
              "Democratic Advancement"                                  1187
              "ATN"                                                     136
              "Bloc Québécois"                                          821144
              "Radical Marijuana"                                       1557
              "Rhinoceros"                                              7263
              "PACT"                                                    91
              "NDP-New Democratic Party"                                3469368
              "Marxist-Leninist"                                        8838
              "The Bridge"                                              122
              "Green Party"                                             602933
              "Canada Party"                                            271}
    :stats   {"Total Votes/Total des votes"        17711983
              "Electors/Électeurs"                 25939742
              "Rejected Ballots/Bulletins rejetés" 120515}
    :seats   {"NDP-New Democratic Party" 44
              "Green Party"              1
              "Conservative"             99
              "Liberal"                  184
              "Bloc Québécois"           10}}
   :2011 {:results {"Libertarian"                        6002
                    "No Affiliation"                     9486
                    "United Party"                       293
                    "Communist"                          2894
                    "Pirate Party"                       3197
                    "CHP Canada"                         18910
                    "CAP"                                1951
                    "Conservative"                       5835270
                    "Liberal"                            2783076
                    "Independent"                        63375
                    "PC Party"                           5790
                    "Animal Alliance/Environment Voters" 1344
                    "Radical Marijuana"                  1756
                    "Rhinoceros"                         3800
                    "NDP-New Democratic Party"           4512411
                    "WBP"                                751
                    "Marxist-Leninist"                   9925
                    "Bloc Qu�b�cois"                     891425
                    "Green Party"                        572095
                    "FPNP"                               229}
          :stats   {"Total Votes/Total des votes"        14823408
                    "Electors/�lecteurs"                 24257592
                    "Rejected Ballots/Bulletins rejet�s" 99428}
          :seats   {"NDP-New Democratic Party" 103
                    "Conservative"             166
                    "Liberal"                  34
                    "Bloc Qu�b�cois"           4
                    "Green Party"              1}}
   :2008 {:results {nil                        0
                    "neorhino.ca"              2122
                    "Libertarian"              7300
                    "No Affiliation"           5457
                    "AAEV Party of Canada"     527
                    "Communist"                3572
                    "Work Less Party"          425
                    "CAP"                      3455
                    "Conservative"             5209069
                    "Liberal"                  3633185
                    "Independent"              89387
                    "Christian Heritage Party" 26475
                    "PC Party"                 5860
                    "PPP"                      186
                    "Radical Marijuana"        2298
                    "NL First Party"           1713
                    "NDP-New Democratic Party" 2515288
                    "WBP"                      195
                    "Marxist-Leninist"         8565
                    "Bloc Qu�b�cois"           1379991
                    "Green Party"              937613
                    "FPNP"                     1611}
          :stats   {"Total Votes/Total des votes"        13929093
                    "Electors/�lecteurs"                 23677639
                    "Rejected Ballots/Bulletins rejet�s" 94799}
          :seats   {"Liberal"                  77
                    "NDP-New Democratic Party" 37
                    "Conservative"             143
                    "Independent"              2
                    "Bloc Qu�b�cois"           49}}
   :2006 {:results {"Libertarian"              3002
                    "No Affiliation"           5164
                    "Canadian Action"          6102
                    "Communist"                3022
                    "Conservative"             5374071
                    "Liberal"                  4479415
                    "Independent"              76696
                    "Christian Heritage Party" 28152
                    "PC Party"                 14151
                    "N.D.P."                   2589597
                    "WBP"                      1094
                    "Marxist-Leninist"         8980
                    "Bloc Qu�b�cois"           1553201
                    "Green Party"              664068
                    "AACEV Party of Canada"    72
                    "Marijuana Party"          9171
                    "FPNP"                     1201}
          :stats   {"Total Votes/Total des votes"        14908703
                    "Electors/�lecteurs"                 23054615
                    "Rejected Ballots/Bulletins rejet�s" 91544}
          :seats   {"Liberal"        103
                    "Conservative"   124
                    "N.D.P."         29
                    "Bloc Qu�b�cois" 51
                    "Independent"    1}}})


(def colours
  {"Conservative"             "#6495ED"
   "Liberal"                  "#EA6D6A"
   "Bloc Québécois"           "#87CEFA"
   "Bloc Qu�b�cois"           "#87CEFA"
   "NDP-New Democratic Party" "#F4A460"
   "N.D.P."                   "#F4A460"
   "Green Party"              "#99C955"
   "Independent"              "purple"
   "Abstentions"              "#666"
   "Spoilt"                   "red"
   "Other"                    "grey"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Data Cleaning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn spoilt-ballots [stats]
  (or (get stats "Rejected Ballots/Bulletins rejetés")
      (get stats "Rejected Ballots/Bulletins rejet�s")))


(defn electors [stats]
  (or (get stats "Electors/Électeurs") (get stats "Electors/�lecteurs")))

(defn cast-ballots [stats]
  (get stats "Total Votes/Total des votes"))

(defn simple-proportions
  "Converts raw election numbers into percentages of the vote per party."
  [{:keys [results stats]}]
  (let [total (cast-ballots stats)]
    (into {} (map (fn [[k v]] [k (/ v total)]) results))))


(defn proportions
  "Same as simple-proportions, but also counts abstentions and spoilt ballots."
  [{:keys [results stats] :as data}]
  (let [cast (cast-ballots stats)
        simple-results (simple-proportions data)
        total (electors stats)
        ratio (/ cast total)]
    (into {"Abstentions" (/ (- total cast) total)
           "Spoilt" (/ (spoilt-ballots stats) total)}
          (map (fn [[k v]] [k (* v ratio)]) simple-results))))


(defn sum-tail
  "Take the top n - 1 results, and sum the tail into an Other bucket."
  [n results]
  (let [[high low] (->> results
                        (sort-by second)
                        reverse
                        (split-at (dec n)))]
    (into {"Other" (reduce + (map second low))}
          high)))

(defn seat-proportions [{:keys [seats]}]
  (let [total (reduce + (vals seats))]
    (into {} (map (fn [[k v]] [k (/ v total)]) seats))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Histogram
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def textline
  (assoc core/text :style {:font "16px serif"}))

(def grid-lines
  (let [horizontal (assoc line :from [0 0] :to [480 0])
        vertical (assoc line :from [10 500] :to [10 0])]
    (with-style {:stroke :darkgrey}
      [; axis
       vertical horizontal
       ;; Horizontal bars
       (map (fn [[caption offset]]
              (-> [(-> textline (assoc :text caption) (translate [-30 -5]))
                   horizontal]
                  (translate offset)))
            [["50%" [0 500]]
             ["40%" [0 400]]
             ["30%" [0 300]]
             ["20%" [0 200]]
             ["10%" [0 100]]])])))

(def bar
  (-> core/rectangle
      (assoc :style {:stroke :none
                     :fill :magenta}
             :width 50
             :height 1000)))

(defn histogram [[year results]]
  [(translate grid-lines [-20 0])
   (-> textline
       (assoc :text (name year))
       (translate [70 -20])
       (scale 3))
   (map-indexed (fn [i [k v]]
                  (-> bar
                      (core/style {:fill (get colours k :magenta)})
                      (scale [1 v])
                      (translate [(* i 60) 0])))
                (->> results
                     (sum-tail 8)
                     (sort-by second)
                     (filter (fn [[k v]] (< 0 v)))
                     reverse))])

(defn summary [election-data]
  (->> election-data
       (map histogram)
       (map-indexed (fn [i s] (translate s [(* i 550) 0])))))

(defn apv [m f]
  (map (fn [[k v]] [k (f v)]) m))

(defn title [text]
  (-> textline
      (assoc :text text)
      (scale 3)
      (translate  [-500 250])))

(defn election [data]
  [[(title "Commons") (summary (apv data seat-proportions))]
   (-> [(title "Proportional") (summary (apv data simple-proportions))]
       (translate [0 600]))
   (-> [(title "With Abstentions") (summary (apv data proportions))]
        (translate [0 1200]))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Pies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(core/deftemplate ::annular-wedge
  {:style        {}
   :inner-radius 1
   :outer-radius 2
   :from         0
   :to           geometry/pi
   :centre       [0 0]}
  (let [fc (geometry/cos from)
        tc (geometry/cos to)
        fs (geometry/sin from)
        ts (geometry/sin to)

        ix1 (* inner-radius fc)
        iy1 (* inner-radius fs)
        ix2 (* inner-radius tc)
        iy2 (* inner-radius ts)

        ox1 (* outer-radius fc)
        oy1 (* outer-radius fs)
        ox2 (* outer-radius tc)
        oy2 (* outer-radius ts)]
    ;; Fun fact, none of the math above is necessary when rendering to canvas
    ;; because of the stateful strokes. I don't want to leave it out though,
    ;; because I don't want to be beholden to canvas.
    (core/path style
               ^:closed [(assoc core/arc
                                :centre centre
                                :radius inner-radius
                                :from from
                                :to to
                                :clockwise? false)
                         (assoc line :from [ix2 iy2] :to [ox2 oy2])
                         (assoc core/arc
                                :centre centre
                                :radius outer-radius
                                :from to
                                :to from
                                :clockwise? true)
                         (assoc line :from [ox1 oy1] :to [ix1 iy1])])))

(defn wedge [party from to]
  "Normalised"
  (assoc annular-wedge
         :inner-radius 5
         :outer-radius 8
         :style {:fill (get colours party :magenta)}
         :from (* 2 geometry/pi from)
         :to (* 2 geometry/pi to)))

(defn ring-election [year accounting]
  (->> election-data
       year
       accounting
       (sort-by second)
       (reduce (fn [{:keys [count shapes]} [party prop]]
                 {:count (+ count prop)
                  :shapes (conj shapes (wedge party count (+ count prop)))})
               {:count 0 :shapes []})
       :shapes))

(defn recursive-rings [f [year & more]]
  (if more
    [(ring-election year f)
     (-> (recursive-rings f more)
         (scale (/ 5 8)))]
    (ring-election year f)))

(def ring-example
  (recursive-rings proportions (keys election-data)))
