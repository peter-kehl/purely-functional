#_(ns minerals
  (:require dbg.main))
;The other side from the same corner has two options: the next (neighbouring) direction (45 degrees), or the second next direction (90 degrees).
(def triangles
;the comments in examples suggest a transformation, but they're an illustration only!
;the return value is the size of the area, or nil.
;Pick a corner point. Take one of 8 directions -> one side of triangle.
;The other side from the same corner has two options: the next (neighbouring) direction (45 degrees), or the second next direction (90 degrees).
(fn [num-per-row]
  (let [mx (vec (for [numeric-line num-per-row]
                   (vec (for [digit (seq (Integer/toString numeric-line 2))]
                      ({\0 false \1 true} digit)))))
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
        turn (fn [prev-dir num-of-turns] {:pre [(direction? prev-dir) (number? num-of-turns)] :post [(direction? %)]} ;previous direction => new direction 45degrees to the right
               (let [prev-dir-index (index-of prev-dir directions)]
                 (numbered-direction (+ prev-dir-index num-of-turns))))
        place? (fn [[x y :as pl]] {:pre [(and (= (count pl) 2) (number? x) (number? y))]}
                 (and (< -1 x height) (< -1 y width)))
        max-side (max height width) ;it's max, not min, because of example of 2 rows: 111, 011.
        move (fn [[from-x from-y :as from] [dir-x dir-y :as dir]] {:pre [(place? from) (direction? dir)]};no :post validation by place?, because we validate outside
               [(+ from-x dir-x) (+ from-y dir-y)])
        ;radius-range (seq (vec (range 0 max-side)))
        immediate-neighbours (fn [from] {:pre [(place? from)]}
                               (filter place?
                                       (for [dir directions]
                                         (move from dir))))
        _ (doseq [row mx]
            (println (apply str (map {true 1, false 0} row))))
        _ (println "x-range" x-range "y-range" y-range "directions" directions)
        triangle-sizes (for [corner-x x-range
                             corner-y y-range
                             :let [corner [corner-x corner-y], _ (println "corner" corner-x corner-y)]
                             :when [(at corner)]
                             dir-left-side  directions
                             ;We only handle 90 degrees at the corner. As we rotate, and we try every place as the "main" corner, that handles all possible triangles.
                             ;Otherwise we need to handle many special cases. E.g. the longest side getting longer by two
                             ;places (instead of just one) at every inner loop iteration. Even though lazy-seq helped to close the triangle, calculating the triangle's
                             ;area would be unnecessarily complex.
                             :let[dir-right-side (turn dir-left-side 2) ;90deg
                                  dir-left-right (turn dir-left-side 3) ;45deg between the left side and the side opposite to "the corner" (i.e. the left to right neighbour side)
                                  #_right-corner-generator #_(fn right-corner-gen [start]
                                                                 (lazy-seq (cons start
                                                                             (let [place (move start dir-right-side)]
                                                                               (if (place? place)
                                                                                 (right-corner-gen place)
                                                                                 ())))))
                                  ;right-corner-seq (right-corner-generator corner)
                                  size (loop [left-corner corner
                                              right-corner corner
                                              size-so-far 1]
                                         (println "outer loop: left-corner" left-corner "right-corner" right-corner "size-so-far" size-so-far "dir-left-side" dir-left-side "dir-right-side" dir-right-side "dir-left-right" dir-left-right)
                                         (let [left-corner-new  (move  left-corner  dir-left-side)
                                               right-corner-new (move right-corner dir-right-side)
                                               left-to-right-length (if (place? right-corner-new)
                                                                       (loop [place left-corner-new ;loop returns nil if the line is not purely mineral
                                                                              length 0]
                                                                          (println "inner loop: place" place "length" length)
                                                                          (if (at place)
                                                                             (if (= place right-corner-new) ;(=...) could be replaced by (index-of place right-corner-seq) to handle when right side get longer by two units (rather than just one unit) per iteration
                                                                                (inc length)
                                                                                (let [place-new (move place dir-left-right)]
                                                                                   (assert (place? place-new) (str "place-new" place-new))
                                                                                   (recur place-new (inc length))))
                                                                             nil)))]
                                           (if left-to-right-length
                                               (recur left-corner-new right-corner-new (+ size-so-far left-to-right-length))
                                               size-so-far)))]
                             :when (<= 3 size)]
                           size )
        ]
    (if (seq triangle-sizes)
        (apply max triangle-sizes)
        nil))))
