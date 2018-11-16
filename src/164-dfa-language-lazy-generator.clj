(def dfa-lang-seq-fn
 (fn dfa-lang-seq [dfa]
   (let [states (dfa :states)
         alphabet (dfa :alphabet)
         start (dfa :start)
         accepts (dfa :accepts)
         transitions (dfa :transitions)
         state? (fn [state] (states state))
         accept? (fn [state] {:pre [(state? state)]}
                     (accepts state))
         letter? (fn [letter] (alphabet letter))
         path? (fn [[letters state]] (and (every? letter? letters)
                                            (state? state)))
         ; Return a new path, with its first part (letters) a seq in reverse (hence reusing subsequences). Return nil the given previous path doesn't accept the given letter.
         path-new (fn [letter sub-path state] {:pre [(path? sub-path) (letter? letter) (state? state)] :post[(path? %)]}
                    (list (cons letter (first sub-path)) state))
         path-state (fn [path] {:pre [(path? path)]}
                      (second path))
         path-to-word (fn [path] {:pre [(path? path)]} ;Paths have letters in reverse (created via cons).
                        (apply str (reverse (first path))))
         contained? (fn [coll item] {:pre [(not= item nil) (not= item false)]} (some (partial = item) coll)) ;the order of params. is for use with (partial ...) to check items. If you'd like its (partial ...) to check collections, reverse the parameters.
         new-split-paths? #_OBSOLETE (fn [[accepted mixcepted] mixcepted-prev] ;two groups of paths, first in accepted states, the second in unaccepted states plus any paths from the first group. This split speeds up delivery. This only applies when adding paths (delta); later on, as accepted paths are taken out, mixcepted may contain paths with accepted states that are not in `accepted` anymore.
                        (and (every? path?  accepted) (every?                            (comp accept? path-state) accepted)
                             (every? path? mixcepted) (every? (some-fn (comp (complement (comp accept? path-state)))
                                                                       (partial contained? accepted) 
                                                                       mixcepted))))
         step-letter-OBSOLETE (fn [[accepted mixcepted :as split-paths]] {:pre [(every? path? accepted) (every? path? mixcepted)] :post [(new-split-paths? % accepted)]} ;split-paths => new split-paths (delta);  accepted only serves for validation in :post
                           "undefined")
         ;--------
         split-paths? (fn [[accepted mixcepted :as path-groups]] ;It accepts two groups of paths, either right from (step-states), or from (step-word). less strict than step-states-result?, because some acceptable states in `mixcepted` may not be in `accepted` anymore.
                        (and (= 2 (count path-groups))
                             (every? (partial every? path?) path-groups)
                             (every? (comp accept? path-state) accepted)
                             (every? (partial contained? mixcepted) accepted))) ;every accepted path is also in mixcepted
         step-states-result? (fn [mixcepted-prev [accepted mixcepted :as path-groups]] ;more strict than (split-paths?), this accepts only from (step-states), when any acceptable states in `mixcepted` are (still) in `accepted`.
                               (and (split-paths? path-groups)
                                    (every? (some-fn (partial contained? accepted) ;every mixcepted path is either in accepted, or is not acceptable as it is. I.e. there are no accepted paths in it other than those also present in `accepted`.
                                                     (complement (comp accept? path-state)))
                                            mixcepted)
                                    (every? (complement (partial contained? mixcepted-prev)) (concat accepted mixcepted))))
         state-transitions? (fn [trans] (and (map? trans) (every? letter? (keys trans)) (every? state? (vals trans)))) ;trans is a per-state submap of transitions map
         step-states (fn [mixcepted-prev] {:pre [(every? path? mixcepted-prev)] :post [(step-states-result? mixcepted-prev %)]} ;step every path one letter forward
                       (reduce (fn [[accepted mixcepted] path-prev]
                                 (let [state-prev (path-state path-prev)
                                       state-transitions (transitions state-prev)
                                       _ (assert (state-transitions? state-transitions))
                                       [accepted-part mixcepted-part] (reduce (fn [[accepted-part mixcepted-part] [letter new-state]]
                                                                                (let [new-path (path-new letter path-prev new-state)
                                                                                      mixcepted-new (cons new-path mixcepted-part)]
                                                                                  (if (accept? new-state)
                                                                                    [(cons new-path accepted-part) mixcepted-new]
                                                                                    [               accepted-part  mixcepted-new])))
                                                                              [()()] state-transitions)]
                                   [(concat accepted accepted-part) (concat mixcepted mixcepted-part)]))
                               [()()] mixcepted-prev))
         step-word-result? (fn [[split-paths word]] (and (split-paths? split-paths) (every? letter? word) (accept? (last word))))
         step-word (fn [[accepted mixcepted :as split-paths]] {:pre [(split-paths? split-paths)] :post [(step-word-result? %)]}
                     (let [accepted-seq (seq accepted)]
                       (if accepted-seq ;return a word, remove from accepted. Every accepted path was also put in unaccepted, hence no need to move it here.
                         "todo")))]
      ;even though a path reached an accepted state, it can also continue
      (lazy-seq (cons dfa (dfa-lang-seq (inc dfa)))))
  ))
