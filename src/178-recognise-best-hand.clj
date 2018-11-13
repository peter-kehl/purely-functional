(require 'clojure.set)

(def best-hand
  (fn [codes]
    (let [code2struc (fn [[suit rank]]
                       {:suit ({\S :spade \H :heart \D :diamond \C :club} suit)
                        ;Rank: 2..9, 10 ("T"), Jack, Queen, King, and Ace -> here 0..12
                        :rank (let [ascii (int rank)]
                                (if (<= ascii (int \9))
                                  (- ascii (int \2))
                                  ({\T 8 \J 9 \Q 10 \K 11 \A 12} rank)))})
          strucs (map code2struc codes)
          _ (println :strucs strucs)
          
          suits (#_dbgf map :suit strucs)
          ranks (#_dbgf map :rank strucs)
          _ (println :ranks ranks)
          
          same-suit (apply = suits)
          each-rank-unique (= (count (into #{} ranks)) 5) ;each rank unique. Instead, could use (distinct ...), or with new CLJ could use (dedupe ..)
          
          standard-sequence (and each-rank-unique
                                 (= (- (apply max ranks) (apply min ranks))
                                    4))
          _ (println :standard-sequence standard-sequence)
          map-by-rank-rev-inv (#_dbgf #_"into" into
                                (#_dbgf sorted-map-by (fn [vec1 vec2] ; reverse; partial order by number of cards per rank, then any consistent (hence by (str ...))
                                                        (if (= (count vec1) (count vec2))
                                                          ;vectors are comparable, but their components - maps - are not! Hence (str ...)
                                                          (#_dbgf compare (str vec2) (str vec1)) ;reverse, hence param vec2 first
                                                          (#_dbgf > (count vec1) (count vec2))))) ;reverse, hence op. >
                                (clojure.set/map-invert (group-by :rank strucs)))
          Xseq-by-rank-rev-inv (sort-by
                                 (comp - count first) ;most frequent rank first; can't use sort-by neither reverse, as those turn a map into a sequence - bad for (vals ...) below
                                 (clojure.set/map-invert
                                   (group-by :rank strucs)))
          _ (println :by-rank-rev-inv map-by-rank-rev-inv)
          nth-frequent-rank-occurrence (fn [pos]
                                         ;(#_dbgf #_"count nth seq" count (key (nth seq-by-rank-rev-inv pos)))
                                         (#_dbgf #_"count nth seq" count (nth (keys map-by-rank-rev-inv) pos)))]
      ;three-of-a-kind (= (nth-frequent-rank-occurrence 0) 3)]
      (cond
        (and same-suit standard-sequence)
        :straight-flush
        
        #_(some (fn [[_ cards]] (= (count cards) 4)) by-rank)
        (= (nth-frequent-rank-occurrence 0) 4)
        :four-of-a-kind
        
        (and (= (count map-by-rank-rev-inv) 2)
             (= (nth-frequent-rank-occurrence 0) 3)) ;the other group must have 2 cards, since we have 2 groups only
        :full-house
        
        same-suit
        :flush
        
        (or standard-sequence
            (let [non-aces (filter #(not= 12 %) ranks)
                  _ (println :non-aces non-aces)]
              (and (= (count non-aces) 4)
                   each-rank-unique
                   (= (apply min non-aces) 0)
                   (= (apply max non-aces) 3))))
        
        :straight
        
        (= (nth-frequent-rank-occurrence 0) 3)
        :three-of-a-kind
        
        (= (nth-frequent-rank-occurrence 0)
           (nth-frequent-rank-occurrence 1)
           2)
        :two-pair
        
        (= (nth-frequent-rank-occurrence 0) 2)
        :pair
        
        :else
        :high-card))))
      
(best-hand ["HA" "D2" "H3" "C9" "DJ"])
(best-hand ["HA" "HQ" "SJ" "DA" "HT"])
(best-hand ["HA" "DA" "HQ" "SQ" "HT"])
(best-hand ["HA" "DA" "CA" "HJ" "HT"]) ;-> :three-of-a-kind
(best-hand ["HA" "DK" "HQ" "HJ" "HT"]) ; straight
(best-hand ["HA" "H2" "S3" "D4" "C5"]) ; straight
(best-hand ["HA" "HK" "H2" "H4" "HT"])
(best-hand ["HA" "DA" "CA" "HJ" "DJ"])
(best-hand ["HA" "DA" "CA" "SA" "DJ"])
(best-hand ["HA" "HK" "HQ" "HJ" "HT"])
