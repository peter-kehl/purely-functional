(require 'clojure.set)
(require 'dbg.main)
;Parenthesis combinations
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

; Following are results (and order of processing) of (parens-stack 3 or 4).
;   v-- indicates swap-point, when generating from top to the bottom
; ((())) 111000
;    v
; (()()) 110100
;  v
; (())() 110010
;    v
; ()(()) 101100
; ()()() 101010
;    v
; (((())))
;     v
; ((()()))
;      v
; ((())())
;   v
; ((()))()
;     v
; (()(()))
;      v
; (()()())
;    v
; (()())()
;      v
; (())(())
;  v
; (())()()
;     v
; ()((()))
;      v
; ()(()())
;    v
; ()(())()
;      v
; ()()(())
; last:
; ()()()()
;
;
; This (transformed) method itself is non-recursive; however, that's only thanks to the fact that each part of the
; resultset (i.e. each prev) contains stack-like info on the current step.   
; The implementation is tail-recursive only to update the resultset.
;
; Start with all openers on the left, all closers on the right.
; Basic steps for generation:
; 1. Iterate (skip) from the end, right to left. Count skipped-closers and skipped-openers.
; 2. You reach the first "switchable" opener when it's an opener and (< (inc skipped-openers) skipped-closers).
;    Swap the opener into a closer. Otherwise (if you reached the leftmost) you finished (and prev was the last generated result).
; 3. Right after the place where you just swapped an opener into a closer:
; 3.1 Append all available openers (plus the one you swapped).
; 3.2 Append all available closers (minus the one you swapped).
; 4. That gives you a result. Repeat for the next result.
; Optimisations:
; 1. Collect, pass & skip the number of consecutive closers on the very right.
; 3. Moved code that generates
; new rightmost consecutive openers & closers to the first loop. Why? Time results from parens-flatter show that splitting
; loops adds much extra overhead. Hence merge loops together.
; Replacing (= XYZ abc) with (zero? abc) and reordering the iteration for it saves little: 605-680ms => 460-660ms
; clj -e "(time (count (filter #(zero? %) (interleave (repeat 1000000 1) (repeat 0)))))"

(def parens-flatish
  ; - prev: the last result
  ; binary 1 for an opener (, 0 for a closer )
  ; Return a seq. of complete solutions as numbers that in binary are of full-length (2x n). No need to return as a set
  ; because the results are already unique.
  ; For n-pairs=12: 620-740ms. (map humanise ...) takes only 40-120ms out of the total.
  ; Added cons-closers => total 480ms. Turned off one last assert => total 460ms.
  (fn [n-pairs]
    (let [n-pairs*2 (* 2 n-pairs)
          n-pairs*2-1 (dec n-pairs*2)]
      (letfn [(digits [numb]
                {:pre [(number? numb)]}
                (vec (reverse (for [i (range 0 n-pairs*2)]
                                (bit-test numb i)))))
              (count-digits [numb digit]
                {:pre [(number? numb) (or (= true digit) (= false digit) #_no-boolean?-in-CLJ-1_4)]}
                (count (filter (partial = digit) (digits numb))))

              ; cons-closers is a group of bits - but 1's instead of 0's - for consecutive closers ) from the very right.
                ; OLD Docs:
                ; Openers and closers contain the respective bits, but:
                ; - both use 1's - openers, and closers, too. (Closers will have them negatedvia bit-and-not.)
                ; - both contain bits, instead of number of bits (as was in parens-flat and parens-flatter).
                ; - both are 0-based - closers, too (it will be shifted later).
                ; Closers are the rightmost. Even though openers are left of closers, this symbol doesn't have them
                ; shifted to the left. That's why we also produce num-of-closers, by which we later shift openers to the left.
              (generate [prev cumulated cons-closers num-of-closers]
                ;OLD assertions:
                (assert (<= num-of-closers n-pairs))
                ;(assert (= num-of-closers (count-digits cons-closers true #_ones-for-closers)))
                (println "prev: " (clojure.pprint/cl-format nil "2r~,'0',B" prev) "cons-closers:" (clojure.pprint/cl-format nil "2r~,'0',B" cons-closers) "num-of-closers:" num-of-closers)
                (assert (= (count-digits prev true) (count-digits prev false) n-pairs) (str "prev: " (clojure.pprint/cl-format nil "2r~,'0',B" prev)))
                (let [[swap-point openers-extra closers+extra num-of-closers+1]
                      (loop [i num-of-closers
                             openers 0
                             closers cons-closers
                             num-of-openers 0
                             num-of-closers num-of-closers]
                        (assert (= i (+ num-of-openers num-of-closers)))
                        (if (bit-test prev i) #_an-opener?
                          (let [num-of-openers+1 (inc num-of-openers)]
                            (if (< num-of-openers+1 num-of-closers) #_swap-point?
                              [i (bit-shift-left openers num-of-closers) closers num-of-closers] ;(list ...) is slower than [...]. CLJ must optimise [...] with destructuring.
                              (if (= n-pairs*2-1 i) #_leftmost-digit?
                                [0 0 0 0] #_we-have-finished
                                (recur (inc i) (bit-set openers num-of-openers)                        closers (inc num-of-openers)      num-of-closers)) #_an-opener-but-not-a-swap-point-yet))
                         ;(recur       (inc i)                         openers (bit-set closers num-of-closers)     num-of-openers  (inc num-of-closers) #_a-closer) #_bit-set-openers-instead-of-clear-for_bit-and-not))]
                           
                          (recur       (inc i)                         openers                         closers     num-of-openers  (inc num-of-closers) #_a-closer) #_bit-set-openers-instead-of-clear-for_bit-and-not))]
                  
                  (println "swap-point:" swap-point "openers-extra:" (clojure.pprint/cl-format nil "2r~,'0',B" openers-extra))
                  (println "closers+extra:" (clojure.pprint/cl-format nil "2r~,'0',B" closers+extra) "num-of-closers+1:" num-of-closers+1)
                  (if (zero? swap-point)
                    cumulated #_finished
                    (let [_ (assert (bit-test prev swap-point)) #_swap-point_is_an_opener
                          num-of-closers (dec num-of-closers+1)
                          closers (bit-clear closers+extra num-of-closers)
                          _ (println "closers:" (clojure.pprint/cl-format nil "2r~,'0',B" closers))
                          ;TODO shift openers before sorting extra?
                          openers (bit-set   openers-extra (dec swap-point))
                          _ (println "openers:" (clojure.pprint/cl-format nil "2r~,'0',B" openers))
                          prev-swapped (bit-clear prev swap-point) #_opener-into==>closer
                          _ (println "prev-swapped:" (clojure.pprint/cl-format nil "2r~,'0',B" prev-swapped))
                          ;openers-shifted (bit-shift-left openers num-of-closers)
                          ;_ (println "openers-shifted:" (clojure.pprint/cl-format nil "2r~,'0',B" openers-shifted))
                          prev-swapped-with-openers (bit-or prev-swapped openers #_-shifted)
                          _ (println "prev-swapped-with-openers:" (clojure.pprint/cl-format nil "2r~,'0',B" prev-swapped-with-openers))
                          value (bit-and-not prev-swapped-with-openers closers)
                          
                          value-OLD '(loop [value (bit-flip prev swap-point) #_opener==>closer
                                            i (dec swap-point) #_>>
                                            openers (inc openers)
                                            closers closers-1]
                                       ;(println "value in loop:" (clojure.pprint/cl-format nil "~,'0',B" value) "openers:" openers "closers:" closers)
                                       (cond
                                         (pos? openers) (recur (bit-set   value i) (dec i) (dec openers)     closers)
                                         (pos? closers) (recur (bit-clear value i) (dec i)      openers (dec closers))
                                         :else  (do
                                                  ;(assert (neg? i))
                                                  value)))]
                      (assert (number? value) (str "value from loop: " value))
                      (recur value (cons value cumulated) closers num-of-closers)))))
              ;(recur value (cons value cumulated) closers-1)
              (humanise [number]
                (clojure.string/replace
                  (clojure.string/replace (java.lang.Long/toBinaryString number) ;toBinaryString is faster 40ms than clojure.pprint/cl-format 308ms.
                    \1 \()
                  \0 \)))
              ; Shift the value by 1 bit. Then set (or not) the lowest bit.
              ; Use with to set first n-pairs bits as openers (, then clear next n-pairs bits as closers )
              ; Not efficient, but it's only for starter. Return a pair (new-value bit) to be used with (iterate....). 
              (shift-and-set [[value do-set]]
                {:pre [(number? value) (or (= true do-set) (= false do-set)) #_no-boolean?-in-CLJ-1_4]}
                (let [shifted (bit-shift-left value 1)
                      new-value (if do-set
                                  (bit-set shifted 0)
                                  shifted)]
                  (list new-value do-set)))]
        (into #{}
          (if (zero? n-pairs)
            ()
            (map humanise
              (let [
                    ; TODO the following runs indefinitely - but only with dbgf
                    ;starter-ones (first (nth ;/---- that "dbgf" causes a runaway. Maybe because iterate is static:?
                    ;                     (dbgf "iterate" iterate #(dbgf shift-and-set %) [0 true]) n-pairs))
                    starter-ones (first (nth
                                          (iterate #(shift-and-set %) [0 true]) n-pairs))
                    ;_ (println "starter-ones" (clojure.pprint/cl-format nil "2r~,'0',B" starter-ones))
                    starter      (first (nth  (iterate shift-and-set [starter-ones false]) n-pairs)) #_see-also-next-assert
                    ;_ (println "starter" (clojure.pprint/cl-format nil "2r~,'0',B" starter))
                    _ (assert (= starter (bit-shift-left starter-ones n-pairs)))
                    _ (assert (= (digits starter) (concat (repeat n-pairs true) (repeat n-pairs false))))]
                
                (generate starter (list starter) starter-ones #_ones-serve-as-closers-for-XOR n-pairs)))))))))

(if (resolve 'time-check) ;from shell: clj -e "(def time-check true)" src/clojurist/
  (println "time for n=12 (which generates " (time (count (parens-flatish 12))) "results"))

;-----------
; OLD, uneffective optimisation:
; parens-flatter is like parens-flat, but it applies rightmost consecutive closers & openers into separate loop. That eliminates a complex conditional.
; However, it's much slower: 390-400ms from shell -> 520-550ms!
(def parens-flatter
  ; - prev: the last result
  ; binary 1 for an opener (, 0 for a closer )
  ; Return a seq. of complete solutions as numbers that in binary are of full-length (2x n). No need to return as a set
  ; because the results are already unique.
  ; For n-pairs=12: 620-740ms. (map humanise ...) takes only 40-120ms out of the total.
  ; Added cons-closers => total 480ms. Turned off one last assert => total 460ms.
  (fn [n-pairs]
    (let [n-pairs*2 (* 2 n-pairs)
          n-pairs*2-1 (dec n-pairs*2)]
      (letfn [(digits [numb]
                ;{:pre [(number? numb)]}
                (vec (reverse (for [i (range 0 n-pairs*2)]
                                (bit-test numb i)))))
              (count-digits [numb digit]
                ;{:pre [(number? numb) (or (= true digit) (= false digit) #_no-boolean?-in-CLJ-1_4)]}
                (count (filter (partial = digit) (digits numb))))
              
              ; cons-closers is the number of consecutive closers ) from the very right.
              (generate [prev cumulated cons-closers]
                ;(assert (and (<= 0 openers) (<= 0 closers) (<= 0 diff) (= diff (- closers openers))))
                ;(assert (or (zero? diff) (pos? closers)))
                ;(println "prev: " (clojure.pprint/cl-format nil "~,'0',B" prev))
                ;(assert (= (count-digits prev true) (count-digits prev true) n-pairs) (str "prev: " prev))
                (let [[swap-point openers closers]
                      (loop [i cons-closers
                             openers 0
                             closers cons-closers]
                        ;(assert (= i (+ openers closers)))
                        (if (bit-test prev i) #_an-opener?
                          (let [openers+1 (inc openers)]
                            (if (< openers+1 closers) #_swap-point?
                              ;(list ...) is slower than [...], because CLJ optimises [...] with destructuring.
                              [i openers closers] ;(list i openers closers)
                              (if (= n-pairs*2-1 i) #_leftmost-digit?
                                [0 0 0] ;(list 0 0 0) #_we-have-finished
                                (recur   (inc i)      openers+1    closers))) #_an-opener-but-not-a-swap-point-yet)
                          (recur         (inc i)      openers (inc closers)) #_a-closer))]
                  
                  (if (zero? swap-point)
                    cumulated #_finished
                    (let [;_ (assert (bit-test prev swap-point)) #_opener
                          closers-1 (dec closers)
                          applied-closers (loop [value prev
                                                 rep closers-1
                                                 i 0]
                                            ;(println "closers: value " (clojure.pprint/cl-format nil "~,'0',B" value) "rep" rep "i" i)
                                            (if (zero? rep)
                                              value
                                              (recur (bit-clear value i) (dec rep) (inc i))))
                          applied-openers (loop [value applied-closers
                                                 rep (inc openers)
                                                 i closers-1]
                                            ;(println "openers: value " (clojure.pprint/cl-format nil "~,'0',B" value) "rep" rep "i" i)
                                            (if (zero? rep)
                                              value
                                              (recur (bit-set value i) (dec rep) (inc i))))
                          value (bit-flip applied-openers swap-point)
                          
                          value-OLD (quote (loop [value (bit-flip prev swap-point) #_opener==>closer]
                                             i (dec swap-point) #_>>
                                             openers (inc openers)
                                             closers closers-1
                                             ;(println "value in loop:" (clojure.pprint/cl-format nil "~,'0',B" value) "openers:" openers "closers:" closers)
                                             (cond
                                               (pos? openers) (recur (bit-set   value i) (dec i) (dec openers)     closers)
                                               (pos? closers) (recur (bit-clear value i) (dec i)      openers (dec closers))
                                               :else  (do
                                                        ;(assert (neg? i))
                                                        value))))]
                      ;(assert (number? value) (str "value from loop: " value))
                      (recur value (cons value cumulated) closers-1)))))
              (humanise [number]
                (clojure.string/replace
                  (clojure.string/replace (java.lang.Long/toBinaryString number) ;toBinaryString is faster 40ms than clojure.pprint/cl-format 308ms.
                    \1 \()
                  \0 \)))
              ; Shift the value by 1 bit. Then set (or not) the lowest bit.
              ; Use with to set first n-pairs bits as openers (, then clear next n-pairs bits as closers )
              ; Not efficient, but it's only for starter. Return a pair (new-value bit) to be used with (iterate....). 
              (shift-and-set [[value do-set]]
                ;{:pre [(number? value) (or (= true do-set) (= false do-set)) #_no-boolean?-in-CLJ-1_4]}
                (let [shifted (bit-shift-left value 1)
                      new-value (if do-set
                                  (bit-set shifted 0)
                                  shifted)]
                  (list new-value do-set)))]
        (into #{}
          (if (zero? n-pairs)
            ()
            (map humanise
              (let [;n-pairs-1 (dec n-pairs)
                    ; TODO the following runs indefinitely - but only with dbgf
                    ;starter-ones (first (nth ;/---- that "dbgf" causes a runaway. Maybe because iterate is static:?
                    ;                     (dbgf "iterate" iterate #(dbgf shift-and-set %) [0 true]) n-pairs))
                    starter-ones (first (nth
                                          (iterate #(shift-and-set %) [0 true]) n-pairs))
                    ;_ (println "starter-ones" (clojure.pprint/cl-format nil "~,'0',B" starter-ones))
                    starter      (first (nth  (iterate shift-and-set [starter-ones false]) n-pairs)) #_see-also-next-assert]
                ;_ (println "starter" (clojure.pprint/cl-format nil "~,'0',B" starter))
                ;_ (assert (= starter (bit-shift-left starter-ones n-pairs)))
                ;_ (assert (= (digits starter) (concat (repeat n-pairs true) (repeat n-pairs false))))]
                
                (generate starter (list starter) n-pairs)))))))))
;------
(def parens-flat
  ; - prev: the last result
  ; binary 1 for an opener (, 0 for a closer )
  ; Return a seq. of complete solutions as numbers that in binary are of full-length (2x n). No need to return as a set
  ; because the results are already unique.
  ; For n-pairs=12: 620-740ms. (map humanise ...) takes only 40-120ms out of the total.
  ; Added cons-closers => total 480ms. Turned off one last assert => total 460ms.
  (fn [n-pairs]
    (let [n-pairs*2 (* 2 n-pairs)
          n-pairs*2-1 (dec n-pairs*2)]
      (letfn [(digits [numb]
                ;{:pre [(number? numb)]}
                (vec (reverse (for [i (range 0 n-pairs*2)]
                                (bit-test numb i)))))
              (count-digits [numb digit]
                ;{:pre [(number? numb) (or (= true digit) (= false digit) #_no-boolean?-in-CLJ-1_4)]}
                (count (filter (partial = digit) (digits numb))))
              
              ; cons-closers is the number of consecutive closers ) from the very right.
              (generate [prev cumulated cons-closers]
                ;(assert (and (<= 0 openers) (<= 0 closers) (<= 0 diff) (= diff (- closers openers))))
                ;(assert (or (zero? diff) (pos? closers)))
                ;(println "prev: " (clojure.pprint/cl-format nil "~,'0',B" prev))
                ;(assert (= (count-digits prev true) (count-digits prev true) n-pairs) (str "prev: " prev))
                (let [[swap-point openers closers]
                      (loop [i cons-closers
                             openers 0
                             closers cons-closers]
                        ;(assert (= i (+ openers closers)))
                        (if (bit-test prev i) #_an-opener?
                          (let [openers+1 (inc openers)]
                            (if (< openers+1 closers) #_swap-point?
                              ;(list ...) is slower than [...], because CLJ optimises [...] with destructuring.
                              [i openers closers] ;(list i openers closers)
                              (if (= n-pairs*2-1 i) #_leftmost-digit?
                                [0 0 0] ;(list 0 0 0) #_we-have-finished
                                (recur   (inc i)      openers+1    closers))) #_an-opener-but-not-a-swap-point-yet)
                          (recur         (inc i)      openers (inc closers)) #_a-closer))]
                  
                  (if (zero? swap-point)
                    cumulated #_finished
                    (let [;_ (assert (bit-test prev swap-point)) #_opener
                          closers-1 (dec closers)
                          value (loop [value (bit-flip prev swap-point) #_opener==>closer
                                       i (dec swap-point) #_>>
                                       openers (inc openers)
                                       closers closers-1]
                                  ;(println "value in loop:" (clojure.pprint/cl-format nil "~,'0',B" value) "openers:" openers "closers:" closers)
                                  (cond
                                    (pos? openers) (recur (bit-set   value i) (dec i) (dec openers)     closers)
                                    (pos? closers) (recur (bit-clear value i) (dec i)      openers (dec closers))
                                    :else  (do
                                             ;(assert (neg? i))
                                             value)))]
                      ;(assert (number? value) (str "value from loop: " value))
                      (recur value (cons value cumulated) closers-1)))))
              (humanise [number]
                (clojure.string/replace
                  (clojure.string/replace (java.lang.Long/toBinaryString number) ;toBinaryString is faster 40ms than clojure.pprint/cl-format 308ms.
                    \1 \()
                  \0 \)))
              ; Shift the value by 1 bit. Then set (or not) the lowest bit.
              ; Use with to set first n-pairs bits as openers (, then clear next n-pairs bits as closers )
              ; Not efficient, but it's only for starter. Return a pair (new-value bit) to be used with (iterate....). 
              (shift-and-set [[value do-set]]
                ;{:pre [(number? value) (or (= true do-set) (= false do-set)) #_no-boolean?-in-CLJ-1_4]}
                (let [shifted (bit-shift-left value 1)
                      new-value (if do-set
                                  (bit-set shifted 0)
                                  shifted)]
                  (list new-value do-set)))]
        (into #{}
          (if (zero? n-pairs)
            ()
            (map humanise
              (let [;n-pairs-1 (dec n-pairs)
                    ; TODO the following runs indefinitely - but only with dbgf
                    ;starter-ones (first (nth ;/---- that "dbgf" causes a runaway. Maybe because iterate is static:?
                    ;                     (dbgf "iterate" iterate #(dbgf shift-and-set %) [0 true]) n-pairs))
                    starter-ones (first (nth
                                          (iterate #(shift-and-set %) [0 true]) n-pairs))
                    ;_ (println "starter-ones" (clojure.pprint/cl-format nil "~,'0',B" starter-ones))
                    starter      (first (nth  (iterate shift-and-set [starter-ones false]) n-pairs)) #_see-also-next-assert]
                ;_ (println "starter" (clojure.pprint/cl-format nil "~,'0',B" starter))
                ;_ (assert (= starter (bit-shift-left starter-ones n-pairs)))
                ;_ (assert (= (digits starter) (concat (repeat n-pairs true) (repeat n-pairs false))))]
                
                (generate starter (list starter) n-pairs)))))))))

;------------
; a recursive (non-tail) method. Deep search-like, back trace. Iterate from the left. Bitwise operations. Pass:
(def parens-stack
; - left: the result so far
; - diff - the unclosed expression depth/the number of currently unclosed openers:
  ;    0 if left has openers & closers balanced, positive if left has more openers.
;   (depth could be determined as (- closers openers). However, we pass it as a param, to save the calculation.) 
; - the number of available (leftover) parens openers & closers
; binary 1 for parens opener (, 0 for for parens closer )
  ; Return a seq. of complete solutions as numbers that in binary are of full-length (2x n). No need to return as a set
  ; at every level, because the results are already unique.
  (fn [target-n]
    (letfn [(level [left diff openers closers #_num-of-available-openers-and-closers]
              ;(assert (and (<= 0 openers) (<= 0 closers) (<= 0 diff) (= diff (- closers openers))))
              ;(assert (or (zero? diff) (pos? closers)))
              (let [left<< (bit-shift-left left 1) #_calculate-left<<-even-if-not-needed-as-the-extra-check-at-every-level-costs-more
                    results (into ;concat was around 1.5x slower than into!
                              (if (pos? openers)
                                (level (bit-set left<< 0) (inc diff) (dec openers)      closers) ; left (
                                ())
                              (if (pos? diff) ;that implies (pos? closers)
                                (level          left<<    (dec diff)      openers  (dec closers)) ;left )
                                ; every leaf finishes with a closer paren )
                                (if (zero? closers)
                                  (list left)#_all-parens-used
                                  ())))]
                results))
            (humanise [number]
               (clojure.string/replace
                 (clojure.string/replace (java.lang.Long/toBinaryString number) ;toBinaryString is faster 40ms than clojure.pprint/cl-format 308ms.
                   \1 \()
                 \0 \)))]
      (into #{}
        (if (zero? target-n)
         ()
         (map humanise
           (level 0 0 target-n target-n)))))))
; the following is the last test. It takes 1.9-2.4sec - too slow!
; But it's slow even if you replace (map humanise...) with (map identity...): 2.2-2.6sec!
; Replacing (man humanise ...) with calling (humanise...) at the leaf level from (level) saves only c.a. 100ms.
;(time (= (nth (sort (parens-stack 12)) 5000) "(((((()()()()()))))(()))"))

    

; current depth
; distance to the match
; ()()
; 1010
; 1111

; (())
; 1210
; 4114

; ((()))
; 123210
; 531135

; (()())
; 121210
; 611116

; ()()()
; 101010
; 111111

; (())()
; 121010
; 411411

; ()(())
; 114114

; (((()())(())))
; 12343432343210
; EC6111164114CE
; C=12, E=14
