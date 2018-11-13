(def three-op-paths
  (fn [start end]
    (let [twice (fn [n] (* 2 n))
          halve (fn [n] (/ n 2))
          plus2 (fn [n] (+ 2 n))
          after-twice (fn [n] (list twice plus2))
          gen-after-op-possibly-with-halve (fn [other-ops] {:pre [(seq? other-ops) (every? fn? other-ops) (not (some (partial = halve) other-ops))]}
                                             (fn [n]
                                               (if (zero? (rem n 2))
                                               (cons halve other-ops)
                                               other-ops)))
          after-halve (gen-after-op-possibly-with-halve (list plus2))
          after-plus2 (gen-after-op-possibly-with-halve (list plus2 twice))
          next-ops (fn [prev-op prev-num] ;return a seq. with some of the above functions
                     (({twice after-twice, halve after-halve, plus2 after-plus2} prev-op) prev-num))
          n+ops? (fn [[n ops :as whole]] (and (= (count whole) 2) (number? n) ops #_<-because-every?-returns-true-for-coll-being-nil (every? fn? ops)))
          num-of-steps (loop [covered #{start} ;numbers (immediate results) already covered
                              recent-nums-and-next-ops #{[start (after-plus2 start)]}; Set of [num seq-of-next-ops]. Not a map {num seq-of-next-ops}, because the same number may be reached multiple times, with different applicable next-ops.
                              num-of-steps 1]
                         (assert (every? n+ops? recent-nums-and-next-ops))
                         (if (covered end)
                           num-of-steps #_result
                           (let [nums+ops (reduce (fn [nums+ops [recent-num available-ops :as n+ops]]
                                                    {:pre [(seq? available-ops) (every? n+ops? nums+ops) (n+ops? n+ops)]
                                                     :post [(every? n+ops? %)]}
                                                    ;(println (type available-ops) available-ops)
                                                    (let [n+ops-subgroup (for [o available-ops
                                                                               :let [n (o recent-num)]
                                                                               :when (not (covered n))]
                                                                            [n (next-ops o n)])]
                                                         (into nums+ops n+ops-subgroup)))
                                                  #{} recent-nums-and-next-ops)
                                 covered-new (into covered (map first nums+ops))]
                             (recur covered-new nums+ops (inc num-of-steps)))
                         ))]
      num-of-steps
    )
   )
  )
