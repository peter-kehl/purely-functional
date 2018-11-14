(def immediate-wins "Return a set of coordinates [x y] of any placement(s), which (each separately) lead to an immediate win. piece is 'o or 'x; mx is a matrix (vector of rows) of symbols 'o, 'x or 'e for empty."
  (fn [piece matrix]
    (let [_ (assert (or (= piece :o)
                        (= piece :x)
                        (= piece :e)))
          has-three? (fn [threesome] (= (count threesome) 3))
          _ (assert (and (has-three? matrix)
                         (every? has-three? matrix)))
          place? (fn [[x y :as coords]]
                   (and (= (count coords) 2) (<= 0 x 2) (<= 0 y 2)))
          at (fn [matrix place] {:pre [(place? place)]} (get-in matrix place))
          triples (apply concat ; ->seq of seq of place
                       '(([0 0] [1 1] [2 2])  ;diagonal from the top left
                         ([2 0] [1 1] [0 2])) ;diagonal from the bottom left
                       (for [i (range 0 3)]
                         `(([~i 0] [~i 1] [~i 2])    ;horizontal: i-th row
                           ([0 ~i] [1 ~i] [2 ~i])))) ;vertical: i-th column
          triple? (fn [triple] (and (= (count triple) 3)
                                    (every? place? triple)))
          _ (assert (every? triple? triples))
          won-triple? (fn [matrix triple] {:pre [(triple? triple)]}
                        (let [matrix-at (partial at matrix)]
                          (if (apply = (map matrix-at triple))
                            (let [value (matrix-at (first triple))] ;All three pieces are equal, hence take the first one.
                            (#{:o :x} value))))) ;Assuming no one won earlier, we don't need to check who won, since we made the move
          won-matrix? (fn [matrix]
                        (some (partial won-triple? matrix) triples))
          empty-places (filter (comp (partial = :e) (partial at matrix)) (for [x (range 0 3), y (range 0 3)] [x y]))
          result (into #{}
                   (for [place empty-places
                         :let [matrix-updated (assoc-in matrix place piece)]
                         :when (won-matrix? matrix-updated)]
                     place))]
      result)))










(immediate-wins :x [[:o :e :e]
                    [:o :x :o]
                    [:x :x :e]])
