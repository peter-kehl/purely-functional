(def word-chains
  (fn [wset] ;1. Create map {word derivative(s)}. 2. Find a chain.
    (let  [;ws (vec wset)
           ;close-slices
           close-unused (fn [one two] ;return true if words are 1 diff apart
                         (let [one-count (count one)
                               two-count (count two)
                               one-chars (seq one)
                               two-chars (seq two)
                               char-pairs (partition 2 (interleave one-chars two-chars))
                               ]
                           ))
           close-words (fn [long short] ;Length of strings doesn't matter, this will reorder them as needed
                         (let [long-count (count long)
                               short-count (count short)]
                           (if (< long-count short-count)
                             (recur short long)
                             (let [same-count (= long-count short-count)]
                              (if (< 1 (- long-count short-count))
                                false
                                (loop [i-long 0
                                       i-short 0
                                       changed? false] ;whether a change was simlated already
                                  (let [more-long (< i-long long-count)
                                        more-short (< i-short short-count)]
                                    (if (not= more-long more-short)
                                      false #_finished
                                      (if (not more-long)
                                        true #_finished
                                        (let [same-char (= (nth long i-long) (nth short i-short))]
                                          (if same-char
                                            (recur (inc i-long) (inc i-short) changed?)
                                            (if changed?
                                              false #_finished
                                              ;if (not same-count), then you have two choices: insert to the short, or remove from the long.
                                              ; *X*            *X* =>shorten=> **
                                              ; *X* <=insert<= **              **
                                              ))
                                    )))))
                                )))))
           ders (reduce ;ders will be a map of derivatives: {from1 [to1-1 to1-2 to1-3...] from2 [to2-1 to2-2...}
                  (fn [mp [from to]] {:pre[(every? (fn [[from to-s]] (and (string? from) (every? string? to-s))) mp)]}
                    (merge mp
                      (if (mp from)
                        {from (conj (mp from) to)}
                        {from to})))
                  {}
                  (concat
                    (for [from wset
                          to wset
                          :when (not= from to)
                          :when (close-words from to)]
                        [[from to]]
                        )))
           _ (println ders)]
          (loop [words-todo-in-stack-above wset;ws
                 words-todo-this-level wset;ws
                 from nil
                 is-1st-level true]
            (assert (= (nil? from) is-1st-level))
            (if is-1st-level
              false; (recur words-todo-in-stack-above (rest words-todo-this-level) (first words-todo-this-level) false)
              
              (let [words-todo (remove #{from} words-todo-this-level)
                    unknown      (cond
                                   (ders from) true
                                   (empty? words-todo-this-level #_maybe) false)]))
            true
            (empty? words-todo-in-stack-above ) false)))
            ;:else (recur (rest words-todo-in-stack-above
   )
(word-chains #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})
  
