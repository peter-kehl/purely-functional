(def winner
  (fn [mx] ;matrix of symbols
    (let [place? (fn [[x y :as coords]]
                   (and (= (count coords) 2) (<= 0 x 2) (<= 0 y 2)))
          at( fn [pl] {:pre[(place? pl)]} (get-in mx pl))
          directions (apply concat #_->seq-of-seq-of-places
                            '(([0 0] [1 1] [2 2]) ;diagonal from top left
                              ([2 0] [1 1] [0 2]))
                            (for [i (range 0 3)]
                              `(([~i 0] [~i 1] [~i 2]) ;horizontal
                                ([0 ~i] [1 ~i] [2 ~i])) ;vertical
                              ))
          direction? (fn [dir] (and
                                (= (count dir) 3)
                                (every? place? dir)))
          _ (println directions)
          _ (assert (every? direction? directions))
          won? (fn [dir] {:pre [(direction? dir)]}
                 (if (apply = (map at dir))
                   (let [value (at (first dir))]
                     (#{:o :x} value))))
          result (some won? directions)]
        result
      )
    )
  )
