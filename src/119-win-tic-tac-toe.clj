(def immediate-win ;each placement (on its own) must lead to an immediate win
  (fn [piece mx] ;piece is 'o or 'x; mx is a matrix of symbols
    (let [place? (fn [[x y :as coords]]
                   (and (= (count coords) 2) (<= 0 x 2) (<= 0 y 2)))
          at( fn [matrix pl] {:pre [(place? pl)]} (get-in matrix pl))
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
          _ (assert (every? direction? directions))
          empty-places (filter (comp (partial = :e) (partial at mx)) (for [x (range 0 3), y (range 0 3)] [x y]))
          won-dir? (fn [mx dir] {:pre [(direction? dir)]}
                     (let [mx-at (partial at mx)]
                       (if (apply = (map mx-at dir))
                         (let [value (mx-at (first dir))]
                           (#{:o :x} value))))) ;we don't need to check who won, since we made the move
          won-mx? (fn [mx]
                    (some (partial won-dir? mx) directions))
          result (into #{}
                    (for [pl empty-places
                          :let [mx-updated (assoc-in mx pl piece)]
                          :when (won-mx? mx-updated)]
                      pl))
          ]
        result
      )
    )
  )
