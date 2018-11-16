(def dfa-seq
 (fn dfa-seq [dfa]
   (let [states (dfa :states)
         alphabet (dfa :alphabet)
         start (dfa :start)
         accepts (dfa :accepts)
         transitions (dfa :transitions)
         state? (fn [state] (states state))
         accept? (fn [state] ;{:pre [(state? state)]}
                     (accepts state))
         letter? (fn [letter] (alphabet letter))
         path? (fn [[letters state]] (and (every? letter? letters)
                                            (state? state)))
         ; Return a new path, with its first part (letters) a seq in reverse (hence reusing subsequences). Return nil the given previous path doesn't accept the given letter.
         path-new (fn [letter sub-path state] ;{:pre [(path? sub-path) (letter? letter) (state? state)] :post[(path? %)]}
                    (list (cons letter (first sub-path)) state))
         path-state (fn [path] ;{:pre [(path? path)]}
                      (second path))
         path-to-word (fn [path] ;{:pre [(path? path)]} ;Paths have letters in reverse (created via cons).
                        (apply str (reverse (first path))))
         contained? (fn [coll item] ;{:pre [(not= item nil) (not= item false)]}
                      (some (partial = item) coll)) ;the order of params. is for use with (partial ...) to check items. If you'd like its (partial ...) to check collections, reverse the parameters.
         new-split-paths? #_OBSOLETE (fn [[accepted mixcepted] mixcepted-prev] ;two groups of paths, first in accepted states, the second in unaccepted states plus any paths from the first group. This split speeds up delivery. This only applies when adding paths (delta); later on, as accepted paths are taken out, mixcepted may contain paths with accepted states that are not in `accepted` anymore.
                        (and (every? path?  accepted) (every?                            (comp accept? path-state) accepted)
                             (every? path? mixcepted) (every? (some-fn (comp (complement (comp accept? path-state)))
                                                                       (partial contained? accepted) 
                                                                       mixcepted))))
         step-letter-OBSOLETE (fn [[accepted mixcepted :as split-paths]] ;{:pre [(every? path? accepted) (every? path? mixcepted)] :post [(new-split-paths? % accepted)]} ;split-paths => new split-paths (delta);  accepted only serves for validation in :post
                           "undefined")
         ;--------
         ;even though a path reached an accepted state, it can also continue
         split-paths? (fn [[accepted mixcepted :as path-groups]] ;It accepts two groups of paths, either right from (step-states), or from (step-word). less strict than step-states-result?, because some acceptable states in `mixcepted` may not be in `accepted` anymore.
                        (and (= 2 (count path-groups))
                             (every? (partial every? path?) path-groups)
                             (every? (comp accept? path-state) accepted)
                             (every? (partial contained? mixcepted) accepted))) ;every accepted path is also in mixcepted
         step-states-result? (fn [mixcepted-prev [accepted mixcepted :as path-groups]] ;more strict than (split-paths?), this accepts only from (step-states), when any acceptable states in `mixcepted` are (still) in `accepted`.
                              (or
                               (nil? path-groups)
                               (and (split-paths? path-groups)
                                    (every? (some-fn empty?
                                                     (partial contained? accepted) ;every mixcepted path is either in accepted, or is not acceptable as it is. I.e. there are no accepted paths in it other than those also present in `accepted`.
                                                     (complement (comp accept? path-state)))
                                            mixcepted)
                                    (every? (complement (partial contained? mixcepted-prev)) (concat accepted mixcepted)))))
         state-transitions? (fn [trans] (and (map? trans)
                                             (every? letter? (keys trans))
                                             (every? state? (vals trans)))) ;trans is a per-state submap of transitions map
         step-states (fn [mixcepted-prev] ;{:pre [(every? path? mixcepted-prev)] :post [(step-states-result? mixcepted-prev %)]} ;step every path one letter forward
                      (let [[accepted mixcepted :as both]
                       (reduce (fn [[accepted mixcepted] path-prev]
                                 (let [state-prev (path-state path-prev)
                                       state-transitions (transitions state-prev {})
                                       _ (assert (state-transitions? state-transitions))
                                       [accepted-part mixcepted-part] (reduce (fn [[accepted-part mixcepted-part] [letter new-state]]
                                                                                (let [new-path (path-new letter path-prev new-state)
                                                                                      mixcepted-new (cons new-path mixcepted-part)]
                                                                                  (if (accept? new-state)
                                                                                    [(cons new-path accepted-part) mixcepted-new]
                                                                                    [               accepted-part  mixcepted-new])))
                                                                              [()()] state-transitions)]
                                   [(concat accepted accepted-part) (concat mixcepted mixcepted-part)]))
                               [()()] mixcepted-prev)]
                       (if (or (seq accepted) (seq mixcepted))
                         both
                         nil)))
         step-word-result? (fn [[split-paths word :as all]] (or (nil? all)
                                                                (and (split-paths? split-paths)(every? (comp letter? symbol str) (seq word))
                                                                     (string? word))
                                                                #_(println "split" split-paths "word" word))) ;word-is-in-human-friendly-order already
         step-word (fn step-word [[accepted mixcepted :as split-paths]] ;nil if no more words
                     ;{:pre [(split-paths? split-paths)] :post [(step-word-result? %)]}
                     (let [accepted-seq (seq accepted)]
                       (if accepted-seq ;return a word, remove from accepted. Every accepted path was also put in unaccepted, hence no need to move it here.
                         [[(rest accepted-seq) mixcepted] (path-to-word (first accepted-seq))]
                         (let [[accepted-new mixcepted-new :as both]
                               (loop [mixcepted mixcepted]
                                 (let [[accepted-new mixcepted-new :as both] (step-states mixcepted)]
                                   (if both
                                     (let [accepted-new-seq (seq accepted-new)]
                                       (if accepted-new-seq
                                         both
                                         (recur mixcepted-new)))
                                     nil)))]
                           (if both
                             (step-word both) ;not (recur ...), because :post prevents tail recursion.
                             nil
                             )))))
         starter-path (list () start)
         _ (assert (path? starter-path))
         starter-groups (list
                         (if (accept? start)
                           (list starter-path)
                           ())
                         (list starter-path))
         lazy-words (fn lazy-words [split-paths]
                      (lazy-seq (let [[split-paths-new word] (step-word split-paths)]
                                  (if word
                                    (cons word (lazy-words split-paths-new))
                                    ()))))]
     (lazy-words starter-groups)))
  )
