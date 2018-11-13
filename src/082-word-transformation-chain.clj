(def word-chains
  (fn [wset] ;1. Create map {word [derivative(s)..]}. 2. Find a chain.
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
           close-words? (fn close-words? [long short] ;Length of strings doesn't matter, this will reorder them as needed
                         {:pre [(string? long) (string? short)] :post [(or (= true %)(= false %))]}
                         (let [long-count  (count long)
                               short-count (count short)]
                           (if (< long-count short-count)
                             (close-words? short long)
                             (let [same-count (= long-count short-count)]
                              (if (< 1 (- long-count short-count))
                                false
                                (loop [i-long 0
                                       i-short 0
                                       changed? false] ;whether a change was simlated already
                                  (let [more-long  (< i-long long-count)
                                        more-short (< i-short short-count)]
                                    (if (not= more-long more-short)
                                      (not changed?) ;finished with short. If we changed already, the leftover char in long can't be delivered
                                      (if (not more-short)
                                        true #_finished
                                        (let [same-char (= (nth long i-long) (nth short i-short))]
                                          (if same-char
                                            (recur (inc i-long) (inc i-short) changed?)
                                            (if changed?
                                              false #_finished
                                              (if same-count
                                                (recur (inc i-long) (inc i-short) true) ;Simulate replacement of a char.
                                                (recur (inc i-long)      i-short  true)) ;Simulate removal from long. The same as if we inserted to short:
                                              ;if (not same-count), then you have two choices: insert to the short, or remove from the long. Equivalent.
                                              ; *X*            *X* =>shorten=> **
                                              ; *X* <=insert<= **              **
                                              ))
                                    )))))
                                )))))
           derivatives? (fn [mp] (and (map? mp) (every? (fn [[from to-s]] (and (string? from) (seq? to-s) (every? string? to-s))) mp)))
           ders (reduce ;ders will be a map of derivatives: {from1 [to1-1 to1-2 to1-3...] from2 [to2-1 to2-2...}
                  (fn [mp [from to]] {:pre[(derivatives? mp)(string? from)(string? to)]}
                    (merge mp
                      (let [existing (mp from)]
                        (if existing
                          {from (cons to existing)}
                          {from (list to)}))))
                  {}
                    (for [from wset
                          to wset
                          :when (not= from to)
                          :when (close-words? from to)]
                        [from to]
                        ))
           _ (println ders)
           starter-word (first wset)] ;Pick up a starter word. Any will do, because every word has to be on the path.
      ;Each word must be used exactly once per path. We keep the most recently reached words, and leftover words for each of them.
      (loop [reached+leftovers [[starter-word (seq (disj wset starter-word))]]] ;seq of [last-reached-word (leftover-words...)]. Not a map, because the same word may be reached through different paths.
        (println reached+leftovers)
        (assert (every? string? (map first reached+leftovers)))
        (assert (every? (partial every? string?) (map second reached+leftovers)))
        (let [reached+leftovers-new (for [[reached leftovers] reached+leftovers
                                          reached-next (ders reached ())]
                                      (let [leftovers-next (remove (partial = reached-next) leftovers)]
                                        (println "\n" leftovers-next)
                                        (list reached-next leftovers-next)
                                      ))
              reached+leftovers-new-seq (seq reached+leftovers-new)]
          (if reached+leftovers-new-seq
            (recur reached+leftovers-new-seq)
            (not (every? empty? (map second reached+leftovers)))))
      )
          #_(loop [words-todo-in-stack-above wset;ws
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
  
