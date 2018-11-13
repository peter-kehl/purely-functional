(require 'clojure.set)
(require 'clojure.pprint)
(require 'dbg.main)

(def veitch
  (fn [rules-orig]
    (let [upper-case? (fn [sym] (let [st (str sym)]
                                  (= (clojure.string/upper-case st) st)))
          ;TODO To optimise, replace sorted-map with hash-map, sorted-set with hash-set. Sorting is only for output when debugging.
          new-map sorted-map
          new-set sorted-set
          full-rules (into #{} ;A set of maps, each {upper-case-symbol boolean, ...} for all fields (based on symbols).
                       (map
                         (fn [rule]
                           (into (new-map) (map
                                      (fn [sym]
                                        (let [turned-on (upper-case? sym)]
                                          [(clojure.string/upper-case sym) turned-on]))
                                      rule)))
                         rules-orig))
          ;_ (println "full-rules:" full-rules)
          fields (map ;a seq. of upper-case strings of all possible symbols. Easy, because
                   ;every set in sets-orig contains all symbols (either on/off - uppercase/lowercase).
                   #(clojure.string/upper-case %)
                   (first rules-orig))
          ;_ (println "fields:" fields)
          ;In following map-based structures, upper-case strings map to boolean.  Because of false, use contains? or nil? to detect relevance (presence) of a field. Don't use (map-instance field-upper-case-name) to determine presence of  field.
          ;;2D map of sets of maps: { upper-case { boolean-for-that-symbol #{set of maps, one map per each rule having that boolean-for-that-symbol as a value for symbol upper-case, each map in the same format as entries in full-rules, but excluding that upper-case field itself: {other-uppercase boolean, ...}, ...}
          field-value-unshort-partials (reduce 
                               (fn [res full-rule] ;For values true/false for every field and for every full rule, generate partial rules (that don't contain the same field). Store them per that "routing" field and per true/false.
                                 (loop [routing-field (first fields)
                                        next-fields (next fields)
                                        res res]
                                      (let [routing-value (full-rule routing-field)
                                            partial-rules (get-in res [routing-field routing-value] #{}#_not-a-sorted-set)
                                            new-res (assoc-in res [routing-field routing-value] (conj partial-rules (dissoc full-rule routing-field)))]
                                        (if next-fields
                                          (recur (first next-fields) (next next-fields) new-res)
                                          new-res))))
                               (new-map)
                               full-rules)
          ;_ (do (println "field-value-unshort-partials:") (clojure.pprint/pprint field-value-unshort-partials))
          ;A map {full rule: full rule}. A help structure for a startpoint below.
          full-rules-to-themselves (into {} (map #(vector % %) full-rules))
          humanise (fn [rule] {:pre [(map? rule)]} (apply str (map (fn [[field value]] (if value field (clojure.string/lower-case field))) rule)) )
          humanise-rule2rule (fn [r2r] {:pre [(map? r2r)]} (clojure.string/join "\n"
                                                                  (map (fn [[from to]] (str (humanise from) " " (humanise to))) r2r)))
          ;Where ((field-value-unshort-partials chosen-upper-case) true) and ((field-value-unshort-partials chosen-upper-case) false)
          ;contain *SOME* equal entries, we can
          ;simplify that rule by eliminating that chosen-upper-case from those equal entries (connected rules). That shortens the original rule (it removes chosen-upper-case.) 
                                        ;The same rule can be shortened several times. On those successive shortening operations, the involved partial rules from field-value-unshort-partials don't know that the rule was shortened already. Hence, as we shorten rules, we need to keep a track of the original rule for each shortened one. The starting point is full-rules-to-themselves.
          full-rules-to-short (reduce (fn [full2short field]
                                        (let [unshort-partials-true  (get-in field-value-unshort-partials [field  true] #{})
                                              unshort-partials-false (get-in field-value-unshort-partials [field false] #{})
                                              ;Which entries (maps) in partials-true and partials-false are equal? For those rules eliminae field altogether.
                                              unshort-partials-shared (clojure.set/intersection unshort-partials-true unshort-partials-false)]
                                          (reduce (fn [f2s unshort-partial]
                                                    (let [full-true  (assoc unshort-partial field true)
                                                          full-false (assoc unshort-partial field false)
                                                          _ (assert (= (count full-true) (count full-false) (count fields)))
                                                          prev-short-true  (f2s full-true)
                                                          prev-short-false (f2s full-false)
                                                          new-short-true  (dissoc prev-short-true field)
                                                          new-short-false (dissoc prev-short-false field)]
                                                      (if (= new-short-true new-short-false)
                                                        (let [
                                                          _ (assert (and prev-short-true prev-short-false))
                                                          ;_ (println "Field: " field ", unshort-partial: " (humanise unshort-partial))
                                                          ;_ (println "full-true: " (humanise  full-true) "prev-short-true: "  (humanise prev-short-true) "->" (humanise new-short-true))
                                                          ;_ (println "full-false:" (humanise full-false) "prev-short-false:"  (humanise prev-short-false) "->" (humanise new-short-false))
                                                          f2s-new (assoc f2s full-true new-short-true full-false new-short-false)
                                                          ;_ (println (humanise-rule2rule f2s-new))
                                                              ]
                                                        f2s-new)
                                                       f2s)))
                                                  full2short unshort-partials-shared)
                                            ))
                                      full-rules-to-themselves fields)
          ;_ (do (println "full-to-short:") (clojure.pprint/pprint full-rules-to-short))
          set-of-shortened (into #{} (vals full-rules-to-short))
          ; A general rule covers a specific one, if all fields with their values from the general are in the specific. The specific rule may have other field(s). Hence "covers" is a reflective operation: any rule covers itself.
          covers (fn [general specific]
                   (every? (fn [[field value]]
                             (= (specific field) value))
                           general))
          ; Remove extra field(s) from rule(s). If we have rule AbX, and its "twist" Abx is covered by any other rule, then remove 'X' from AbX => leave Ab.
          without-extra-fields (map (fn [rule]
                                      (reduce (fn [res [field value]]
                                                (let [rule-twisted (conj rule [field (not value)])
                                                      covered? (some (fn [general] (covers general rule-twisted)) set-of-shortened)]
                                                  (if covered?
                                                       res
                                                       (conj res [field value]))))
                                              (new-map) rule))
                                    set-of-shortened)]
      (into #{} (map (fn [mp]
                       (into #{} (map (fn [[field value]]
                                        (symbol (if value
                                                   field
                                                   (clojure.string/lower-case field))))
                                      mp)))
                     without-extra-fields))
      )))
      
    
