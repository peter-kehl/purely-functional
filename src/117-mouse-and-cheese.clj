(require 'clojure.set)
(def mouse-cheese
  (fn [matrix]
    (let [height (count matrix)
          width (count (first matrix))
          place? (fn [[x y :as coords]] (and (vector? coords) (= (count coords) 2) (< -1 x height) (< -1 y width)))
          directions '(#_up [-1 0] #_down [1 0] #_left [0 -1] #_right [0 1])
          direction? (fn [dir] (some (partial = dir) directions))
          move (fn [[x y :as coords] [x-delta y-delta :as dir]] {:pre [(place? coords) (direction? dir)]} ;no post check, since we want it also to return place outside the matrix
                 [(+ x x-delta) (+ y y-delta)])
          neighbours-all (fn [[x y :as from]] {:pre (place? from)}
                           (for [dir directions
                                 :let [to (move from dir)]
                                 :when (place? to) #_within-bounds-only]
                             to)
                           )
          replace-char-at (fn [string index replacement] {:pre [(string? string) (number? index)]
                                                          :post[(string? %) (= (count %) (+ (count string) (count (str replacement)) -1))]}
                            (str (subs string 0 index) replacement (subs string (inc index))))
          result (loop [mx matrix
                   recent-visits (for [x (range 0 height)
                                       y (range 0 width)
                                       :when (= (get (matrix x) y) \M)]
                                   [x y])]
              (println :mx mx)
              (let [at (fn [[x y :as coords]] {:pre (place? coords)} (println :at x y mx)(get (mx x) y))
                    is? (fn [pl character] (= (at pl) character))
                    visited? (fn [pl] (is? pl \M)) ;as we visit places, we mark them with M
                    pass? (fn [pl] (is? pl \space))
                    goal? (fn [pl] (is? pl \C))
                    recent-neighbours (for [visited recent-visits
                                            neighb (neighbours-all visited)]
                                        neighb)
                    _ (println :recent-neighbours recent-neighbours)
                    passable-neighbours (filter pass? recent-neighbours)
                    ]
                (if (seq (filter goal? recent-neighbours))
                  true
                  (if (seq passable-neighbours)
                    (let [mx-with-visits (reduce (fn [mx [x y :as pl]] {:pre [(place? pl)]}
                                                   (let [row (mx x)
                                                         row-updated (replace-char-at row y \M)]
                                                     (assoc mx x row-updated)))
                                             mx passable-neighbours)]
                      (recur mx-with-visits passable-neighbours))
                    #_else false))))
              ]
      result
    )
 )
)
