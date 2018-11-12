(def tic
  (fn [[row0 row1 row2]]
      (let [won
            (fn [[a b c]]
              (or
                  (and
                      (= a b c)
                      (some #{a} [:x :o])
                      a) ;'a' makes (and) return value of a on succes (instead of boolean)
                  nil))] ; 
        
        (or
            (won row0) (won row1) (won row2)
            (let [column
                  (fn [col]
                    [(row0 col) (row1 col) (row2 col)])]
              (or
                  (won (column 0)) (won (column 1)) (won (column 2))
                  (won [(row0 0) (row1 1) (row2 2)])
                  (won [(row0 2) (row1 1) (row2 0)])))))))
(tic [[:e :e :e]
      [:e :e :e]
      [:e :e :e]])
(tic [[:x :e :o]
      [:x :e :e]
      [:x :e :o]])
(tic [[:x :e :o]
      [:x :x :e]
      [:o :x :o]])
;others
(fn [d]
  (let [n [0 1 2]
        s (set
            (concat
              d
              (apply map list d)
              [(for [a n] (get-in d [a a]))
               (for [a n] (get-in d [(- 2 a) a]))]))]
    (cond
      (s [:x :x :x]) :x
      (s [:o :o :o]) :o
      :e nil)))
