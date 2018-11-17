(def triangles
;the comments in examples suggest a transformation, but they're an illustration only!
;the return value is the size of the area, or nil.
;Pick a corner point. Take one of 8 directions -> one side of triangle.
;The other side from the same corner has two options: the next (neighbouring) direction (45 degrees), or the second next direction (90 degrees).
(fn [num-per-row]
  (let [mx (vec (for [numeric-line num-per-row
                      digit (seq (Integer/toString numeric-line 2))]
                   ({\0 false \1 true} digit)))
        ; [x y] x is  row, y is a column
        directions #_from-top-left-clockwise [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1]]
        numbered-direction #_rotate-index-overflow (fn [num] (directions (rem num 8)))
        line (fn [])
        right-of (fn []) ;<<<<
        left-of (fn [])
        height (count mx)
        width (count (first mx))
        x-range (seq (vec (range 0 height)))
        y-range (seq (vec (range 0 width)))
        _ (assert (every? (comp (partial = width) count) mx))
        ;mx is a matrix-like map of maps, with indexes that can be negative: relative to the start point.
        at (fn [[x y :as coordinates]] {:pre [(= (count coordinates) 2)]} #_returns-nil-if-not-set (get-in mx [x y]))
        ;directions as [delta-x delta-y], in coordinates where x is a row, y is a column, [0 0] is the top left corner. D down, U up, R right, L left:
        direction? (fn [[delta-x delta-y :as all]] (and (= (count all) 2) (<= -1 delta-x 1) (<= -1 delta-y 1) (not= delta-x delta-y 0)))
        _ (assert (every? direction? directions))
        index-of (fn [item coll] ;nil if no such item. Item itself can be nil or false.
                   (loop [index 0, s (seq coll)]
                     (if s
                       (if (= (first s) item)
                         index
                         (recur (inc index) (next s)))
                       nil)
                     ))
        turn (fn [prev-dir num-of-turnes] {:pre [(direction? prev-dir) (number? num-of-turns)] :post [(direction? %)]} ;previous direction => new direction 45degrees to the right
               (let [prev-dir-index (index-of prev-dir directions)]
                 (numbered-direction (+ prev-dir-index num-of-turns))))
        place? (fn [[x y :as pl]] {:pre [(and (= (count pl) 2) (number? x) (number? y))]}
                 (and (< -1 x height) (< -1 y width)))
        max-side (max height width) ;it's max, not min, because of example of 2 rows: 111, 011.
        radius-range (seq (vec (range 0 max-side)))
        within-triangle "TODO"
        triangle-sizes (for [corner-x x-range
                             corner-y y-range
                             dir-left-side  directions
                             [dir-right-side dir-left-right] [[(turn dir-left-side 1) (turn dir-left-side 2)] ;45degrees
                                                              [(turn dir-left-side 2) (turn dir-left-side 3)]]];90degrees
                         (for [radius radius-range]))
      ])))
