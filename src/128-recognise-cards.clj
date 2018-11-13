;Suit: Spades, Hearts, Diamonds, and Clubs 
;Rank: 2..9, 10 ("T"), Jack, Queen, King, and Ace -> here 0..12
(def code2struc
  (fn [[suit rank]]
    {:suit ({\S :spade \H :heart \D :diamond \C :club} suit)
     :rank (let [ascii (int rank)]
             (if (<= ascii (int \9))
               (- ascii (int \2))
               ({\T 8 \J 9 \Q 10 \K 11 \A 12} rank)))}))

(code2struc "CA")

