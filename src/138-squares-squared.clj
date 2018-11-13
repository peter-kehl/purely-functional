(def squares-sq
 (fn [num-from num-max]
  (let [nums (take-while #(<= % num-max)
               (iterate #(* % % #_Math.pow-is-for-double-only) num-from))
        ;_ (println "nums:" nums)
        digits (for [n nums
                     d (str n)] d)
        ;_ (println :digits digits)
        matrix? map?
        ;mx is a matrix-like map of maps, with indexes that can be negative: relative to the start point.
        at (fn [mx [x y :as coordinates]] {:pre [(or (matrix? mx) (seq? mx)) (= (count coordinates) 2)]} #_returns-nil-if-not-set (get-in mx [x y]))
        ;set-at (fn [mx x y value] (assoc-in mx [x y] value))
        ;directions as [delta-x delta-y], in coordinates where x is a row, y is a column, [0 0] is the top left corner. D down, U up, R right, L left:
        d-r [1 1], d-l [1 -1], u-l [-1 -1], u-r [-1 1]
        ;directions listed in order, each 45degrees to the right after the previous one.
        directions [d-r d-l u-l u-r]
        directions-reversed (reverse directions) ;_ (assert (= directions-reversed [u-r u-l d-l d-r]))
        dir? (fn [[delta-x delta-y :as all]] (and (= (count all) 2) (<= -1 delta-x 1) (<= -1 delta-y 1) (not= delta-x delta-y 0)))
        turn (fn [prev-dir] {:pre [(dir? prev-dir)]};previous direction => new direction 45degrees to the right
               (if (= prev-dir u-r)
                 d-r
                 (last (for [dir directions-reversed
                              :while (not= dir prev-dir)]
                          dir))))
        _ (assert (and (= (turn u-r) d-r))) _ (assert (= (turn d-r) d-l)) _ (assert (and (= (turn d-l) u-l) (= (turn u-l) u-r)))
        place? (fn [[x y :as pl]] (and (= (count pl) 2) (number? x) (number? y)))
        move (fn [[x y :as pl] [x-delta y-delta :as dir]] {:pre [(place? pl) (dir? dir)]}
               [(+ x x-delta) (+ y y-delta)])
        right-dir-and-neighbour (fn [place dir]
                                  (let [dir-right (turn dir)]
                                    [dir-right (move place dir-right)]))
        place-dir (fn [places place-prev dir-prev] ;return [new-place new-dir] where new-dir may be the same as dir-prev
                        {:pre [(place? place-prev) (dir? dir-prev) #_(or (true? over-two?) (false? over-two?))] :post [(place? (first %)) (dir? (second %))]}
                        (let [[dir-right place-prev-right-neighbour] (right-dir-and-neighbour place-prev dir-prev)
                               ;_ (println :place-prev-right-neighb place-prev-right-neighbour)
                              ]
                            (if (at places place-prev-right-neighbour)
                              (let [place-prev-direct-neighbour (move place-prev dir-prev)]
                                [place-prev-direct-neighbour dir-prev])
                              [place-prev-right-neighbour dir-right])))
        new-row sorted-map ;sorted only for debugging, in production it can be hash-map
        entry? #(or (and (char? %) (<= 48 (int %) 57)) (= % \*))
        fill-in (fn [places [x y :as place] value] {:pre [(matrix? places) (place? place) (entry? value)] :post [(= ((% x)y) value)]} ;like assoc-in, but using new-map
                  (let [row (get places x (new-row))
                        row-updated (assoc row y value)]
                    ;(println :fill-in :place place :value value)
                    (assoc places x row-updated)))
        rows-strings (fn [places]
                       (let [[min-x max-x min-y max-y] (reduce (fn [[min-x max-x min-y max-y] [x y]]
                                                                 [(min min-x x) (max max-x x) (min min-y y) (max max-y y)])
                                                           [10 -10 10 -10]
                                                           (for [[x row] places, [y _] row] [x y]))
                                        ;_ (println "min-x" min-x "max-x" max-x "min-y" min-y "max-y" max-y)
                             rows-with-spaces (for [x (range min-x (inc max-x))]
                                                (apply str
                                                     (for [y (range min-y (inc max-y))]
                                                       (get-in places [x y] \space)
                                                       )))]
                         rows-with-spaces))
        print-places (fn [places] (doseq [row (rows-strings places)] (println (str row \|))))
        ;start with direction up to the right u-r, because place-and-dir will turn it to d-r after the 1st digit.
        ;To complete a square, we need an even number of turns (ignoring the very first "turn" from starting direction u-r).
        places (let [[places-for-digits place-last-digit dir-last-digit num-of-turns]
                     (loop [places (sorted-map 0 (sorted-map 0 (first digits)))
                            place-last [0 0]
                            dir-last u-r
                            num-of-turns -1
                            digits-leftover (next digits)]
                       ;(println :places places ":")
                       ;(print-places places)
                       ;(println :place-last place-last :dir-last dir-last :num-of-turns num-of-turns :digits-leftover digits-leftover)
                       (if digits-leftover
                           (let [[place dir] (place-dir places place-last dir-last)
                                 same-dir? (= dir-last dir)
                                 num-of-turns-new (if same-dir? num-of-turns (inc num-of-turns))
                                 places-new (fill-in places place (first digits-leftover))
                                 ;_ (println :same-dir? same-dir? :num-of-turns-new num-of-turns-new)
                                 ]
                             (recur places-new place dir num-of-turns-new (next digits-leftover) ))
                           [places place-last dir-last num-of-turns])
                       )]
                 (if (= (count digits) 1)
                   places-for-digits
                   (let [
                     ;We need to finish with an even number of turns. However, if the digits were filled with an even number of turns, but the last
                     ;digit didn't have a right neighbour, then that digit was "out of square," and we need two more turns.
                     ;When filling up with stars *, after the very last (even numbered) turn fill only places that have a neihbour on the right.
                     even-turn-but-out-of-square (and (even? num-of-turns)
                                                      (let [[_ last-digit-right-neighbour] (right-dir-and-neighbour place-last-digit dir-last-digit)]
                                                        (not (at places-for-digits last-digit-right-neighbour))))
                     dir-last-digit-adjusted (if even-turn-but-out-of-square
                                               (turn dir-last-digit)
                                               dir-last-digit)
                     num-of-turns-adjusted (if even-turn-but-out-of-square
                                             (inc num-of-turns)
                                             num-of-turns)
                     places (loop [places places-for-digits
                                   pl place-last-digit
                                   dir dir-last-digit-adjusted
                                   even-turns (even? num-of-turns-adjusted)]
                              (let [direct-neighbour (move pl dir)
                                    [dir-right direct-right-neighbour] (right-dir-and-neighbour direct-neighbour dir)]
                                (if (at places direct-right-neighbour)
                                  (let [places-* (fill-in places direct-neighbour \*)]
                                    (recur places-* direct-neighbour dir even-turns))
                                  (if (not even-turns)
                                    (let [[_ right-neighbour] (right-dir-and-neighbour pl dir) 
                                          continue-direct (at places right-neighbour)
                                          pl-direct-or-right (if continue-direct direct-neighbour right-neighbour)
                                          places-* (fill-in places pl-direct-or-right \*)]
                                      (recur places-* pl-direct-or-right dir-right true))
                                    places))))
                     ;_ (print-places places)
                    ]
                 places)))
        ;_ (println :places places)
        ] (rows-strings places))))
