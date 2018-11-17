; This algorithm is neat. However, it works up to n=3 only.
(def parens-sets-strings
  (fn [n]
    (if (zero? n)
      #{""}
      (loop [sub #{"()"}
             n (dec n)]
        (if (zero? n)
          sub
          (recur (reduce (fn [res prev]
                           (conj res
                                 (str "()" prev)
                                 (str \( prev \))
                                 (str prev "()")))
                         #{} sub)
                 (dec n)))))))

(def parens-heterogeneous
  (fn [n]
    (into #{}
      (cond
        (zero? n) ()
        ;(= n 1) '("()")
        :else (map (comp (partial apply str) flatten)
                   (loop [s '("()")
                          n (dec n)]
                     (if (zero? n)
                       s
                       (recur (reduce (fn [res prev]
                                        (cons (cons "()" prev)
                                              (cons (list "(" prev ")")
                                                    (cons (list prev "()")
                                                          res)))) ;benefit from (flatten ...) later
                                      () s)
                              (dec n)))))))))
