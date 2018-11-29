(set! *warn-on-reflection* true)

(require 'primitive-math)
;(primitive-math/use-primitive-operators) ; must be used together with *warn-on-reflection*. It doesn't work without it!

(defmacro assert-prim-long [x] `(== (long ~x) ~x)) ;works with primitive-math/use-primitive-operators and *warn-on-reflection* together

(def shifted-ones (longs (long-array (for [i (range 0 63)]
                                (bit-set 0 i)))))
(defmacro bit-set-prim [x n]
  `(bit-or ~x (aget (longs shifted-ones) ~n)))

(def parens-flat-hint
  ; - prev: the last result
  ; binary 1 for an opener (, 0 for a closer )
  ; Return a seq. of complete solutions as numbers that in binary are of full-length (2x n). No need to return as a set
  ; because the results are already unique.
  ; For n-pairs=12: 620-740ms. (map humanise ...) takes only 40-120ms out of the total.
  ; Added cons-closers => total 480ms. Turned off one last assert => total 460ms.
  ; "int" primitive type hints generate "long", hence I use long directly.
  (fn [^long n-pairs]
    (let [n-pairs*2 (long (* 2 n-pairs))
          n-pairs*2-1 (long (dec n-pairs*2))]
      (letfn [(digits [numb] ;Get binary digits of a number.
                ;{:pre [(number? numb)]}
                (vec (reverse (for [i (range 0 n-pairs*2)]
                                (bit-test numb i)))))
              (count-digits [numb digit]
                ;{:pre [(number? numb) (or (= true digit) (= false digit) #_no-boolean?-in-CLJ-1_4)]}
                (count (filter (partial = digit) (digits numb))))

              ; cons-closers is the number of consecutive closers \) from the very right.
              (generate [^long prev #_no-need-for-typehint cumulated ^long  cons-closers]
                ;(assert (and (<= 0 openers) (<= 0 closers) (<= 0 diff) (= diff (- closers openers))))
                ;(assert (or (zero? diff) (pos? closers)))
                ;(println "prev: " (clojure.pprint/cl-format nil "~,'0',B" prev))
                ;(assert (= (count-digits prev true) (count-digits prev true) n-pairs) (str "prev: " prev))
                (let [[^long swap-point ^long openers ^long closers]
                      (loop [i (long cons-closers)
                             openers (long 0)
                             closers i]
                        ;(assert (= i (+ openers closers)))
                        (if (bit-test prev i) #_an-opener?
                          (let [openers+1 (inc openers)]
                            (if (< openers+1 closers) #_swap-point?
                              ;(list ...) is slower than [...], because CLJ optimises [...] with destructuring.
                              [i openers closers] ;(list i openers closers)
                              (if (= n-pairs*2-1 i) #_leftmost-digit?
                                [(long 0) (long 0) (long 0)] ;(list 0 0 0) #_we-have-finished
                                (recur   (inc i)      openers+1    closers))) #_an-opener-but-not-a-swap-point-yet)
                          (recur         (inc i)      openers (inc closers)) #_a-closer))]
                  (if (zero? swap-point)
                    cumulated #_finished
                    (let [;_ (assert (bit-test prev swap-point)) #_opener
                          closers-1 (dec closers)
                          value
                          #_loop-always-returns-object-hence-cast-its-result
                          (long (loop #_typehint-doesnt-help-right-after-loop-word
                                       ;value is not (long ...), because then bit-set and bit-clear returned an Object anyway. Better keep it as an object so it will be passed to bit-set and bit-clear again.
                                      [#_typhint-didnt-help-here value (bit-flip prev swap-point) #_opener==>closer
                                       i (long (dec swap-point)) #_>>
                                       openers (inc openers)
                                       closers closers-1]
                                  ;(assert-prim-long value)
                                  ;(println "value in loop:" (clojure.pprint/cl-format nil "~,'0',B" value) "openers:" openers "closers:" closers)
                                  (cond
                                    (pos? openers) (recur (bit-set-prim   value i) (dec i) (dec openers)     closers)
                                    (pos? closers) (recur (bit-clear value i) (dec i)      openers (dec closers))
                                    :else  (do
                                             ;(assert (neg? i))
                                             value))))]
                      ;(assert (number? value) (str "value from loop: " value))
                      (recur  value (cons value cumulated) closers-1)))))
              (humanise [number]
                (clojure.string/replace
                  (clojure.string/replace (java.lang.Long/toBinaryString number) ;toBinaryString is fast: 40ms, rather than clojure.pprint/cl-format 308ms.
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

