(ns project-euler.core
  (:require [clojure.java.io :as io]
            [clojure.math [combinatorics :as comb]
                          [numeric-tower :as math]]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn divisors
  "Find all divisors of n"
  [n]
  (let [up-to (Math/sqrt n)]
    (loop [d 1
           smaller-divisors []
           bigger-divisors ()]
      (cond (> d up-to) (if (= (peek smaller-divisors) (peek bigger-divisors))
                          (concat smaller-divisors (pop bigger-divisors))
                          (concat smaller-divisors bigger-divisors))
            (zero? (rem n d)) (recur (inc d)
                                     (conj smaller-divisors d)
                                     (conj bigger-divisors (/ n d)))
            :else (recur (inc d) smaller-divisors bigger-divisors)))))

(defn proper-divisors
  "Find the proper divisors of n"
  [n]
  (drop-last (divisors n)))

(defn abundant-number?
  "Check if n is abundant"
  [n]
  (< n (apply + (proper-divisors n))))

(def abundant-numbers
  "Lazy seq of all abundant numbers"
  (for [n (iterate inc 1)
        :when (abundant-number? n)]
    n))

(defn prime?
  "Check if a number is prime. If the number provided is less than
  zero, check its negation for primeness."
  [n]
  (cond (< n 0) (prime? (- n))
        (< n 2) false
        (= n 2) true
        :else (and (not (zero? (rem n 2)))
                   (not-any? #(zero? (rem n %))
                             (range 3 (+ (Math/sqrt n) 1) 2)))))

(def primes
  "Lazy seq of all primes"
  (filter #(prime? %) (iterate inc 0)))

(defn prime-factors
  "Find the prime factors of n"
  [n]
  (loop [d n
         f {}
         p (first primes)
         ps (rest primes)]
    (cond (or (= d 0) (= d 1)) f
          (zero? (rem d p)) (recur (quot d p)
                                   (assoc f p (inc (get f p 0)))
                                   p
                                   ps)
          :else (recur d f (first ps) (rest ps)))))

(def triangle-numbers
  "Lazy seq of all triangle numbers"
  (map #(bit-shift-right (* (inc %) %) 1) (iterate inc 1)))

(defn integer-sqrt
  "Find the largest integer smaller than square root of n"
  [n]
  (loop [lower 0
         current 1]
    (let [sqr (* current current)]
      (cond (= current lower) current
            (< sqr n) (recur current (bit-shift-left current 1))
            (> sqr n) (recur lower
                             (bit-shift-right (+ current lower) 1))
            :else current))))

(defn digits
  "Find the digits of n"
  [n]
  (map #(Character/digit % 36) (seq (str n))))

(defn digits->number
  "Convert a seq of digits to number"
  [coll]
  (reduce #(+' (*' %1 10) %2) 0 coll))

(defn C
  "nCr"
  [n r]
  (/ (apply *' (range (inc' r) (inc' n)))
     (apply *' (range 2 (inc' (- n r))))))

(defn counts
  "Return a seq of pairs of unique elements in coll and their count"
  [coll]
  (map #(vector (first %) (count %)) (partition-by identity (sort coll))))

(def pentagonal-numbers
  "A lazy seq of all pentagonal-numbers"
  (map #(/ (* % (- (* 3 %) 1)) 2) (iterate inc 1)))

(defn pentagonal-number?
  "Check if n is a pentagonal number"
  [n]
  (= n (first (drop-while #(< % n) pentagonal-numbers))))

(defn problem-27
  "Product of the coefficients, a and b, in (+ (* n n) (* a n) b),
  where (and (< (abs a) 1000) (< (abs b) 1000)) that produces the
  maximum number of primes for consecutive values of n, starting
  with n = 0.
  b must be a prime, as for n = 0, f is prime."
  []
  (letfn [(f [a b n] (+ (* n n) (* a n) b))
          (consecutive-primes [a b]
            (apply + (for [n (iterate inc 0)
                           :while (prime? (f a b n))]
                       1)))
          (compare-triplets [[_ _ n1 :as t1] [_ _ n2 :as t2]]
            (if (> n1 n2) t1 t2))]
    (reduce compare-triplets
      [0 0 0]
      (for [a (range -999 1000)
            b (range -999 1000)]
        [a b (consecutive-primes a b)]))))

(defn problem-26
  "Find the value of (< d 1000) for which (/ 1 d) contains the
  longest recurring cycle in its decimal fraction part."
  []
  (letfn [(recurring-cycle-length [n]
            (loop [frac 1 pos 0 rem-seen {1 0}]
              (let [new-frac (rem (* 10 frac) n)
                    new-pos (inc pos)]
                (cond (zero? new-frac) 0
                      (contains? rem-seen new-frac) (- new-pos
                                                       (rem-seen new-frac))
                      :else (recur new-frac
                                   new-pos
                                   (assoc rem-seen new-frac new-pos))))))
          (compare-pairs [[_ r1 :as p1] [_ r2 :as p2]]
            (if (> r1 r2) p1 p2))]
    (reduce compare-pairs [0 0] (map #(vector % (recurring-cycle-length %))
                                    (range 3 1000)))))

(defn problem-23
  "Find the sum of all the positive integers which cannot be
  written as the sum of two abundant numbers.
  By mathematical analysis, it can be shown that all
  integers greater than 28123 can be written as the sum of two
  abundant numbers"
  []
  (let [upper 28123
        abundants (take-while #(< % upper) abundant-numbers)
        abundant-pair-sums (set (for [x abundants
                                      y abundants
                                      :let [s (+ x y)]
                                      :when (<= s upper)]
                                  s))]
    (apply + (for [n (range 1 upper)
                   :when (not (contains? abundant-pair-sums n))]
               n))))

(defn problem-31
  "Given denominators [1 2 5 10 20 50 100 200]. How many ways are
  there to make change for 200?"
  []
  (let [ds [1 2 5 10 20 50 100 200]
        table (atom (apply hash-map (mapcat #(vector [0 %] 1)
                                            (range (count ds)))))]
    (doseq [money (range 1 201)
            idx (range (- (count ds) 1) -1 -1)]
      (swap! table
             (fn [t] (assoc t [money idx]
                            (if (< money (ds idx))
                              0
                              (+ (get t [(- money (ds idx)) idx] 0)
                                 (get t [money (+ idx 1)] 0)))))))
    (@table [200 0])))

(defn problem-42
  "Count triangular words"
  []
  (let [tri-set (set (take-while #(< % 1000) triangle-numbers))
        tris (vec (take 26 triangle-numbers))
        content (slurp "words.txt")]
    (letfn [(triangular-word? [s]
              (contains? tri-set
                         (apply + (map #(+ 1 (- (int %) (int \a)))
                                       (seq (str/lower-case s))))))]
      (count (filter triangular-word? (re-seq #"[A-Z]+" content))))))

(defn problem-39
  "If p is the perimeter of a right angle triangle with integral length sides,
  {a,b,c} there are exactly three solutions for p = 120.
  {20,48,52}, {24,45,51}, {30,40,50}
  For which value of p ≤ 1000, is the number of solutions maximised?"
  []
  (letfn [(compare-pairs [[_ c1 :as p1] [_ c2 :as p2]]
            (if (> c1 c2) p1 p2))]
    (reduce compare-pairs
      (counts (for [a (range 1 1000)
                    b (range a 1000)
                    :let [c2 (+ (* a a) (* b b))
                          c (integer-sqrt c2)
                          p (+ a b c)]
                    :when (and (= c2 (* c c))
                               (<= p 1000))]
                p)))))

(defn problem-33
  "The fraction 49/98 is a curious fraction, as an inexperienced
  mathematician in attempting to simplify it may incorrectly
  believe that 49/98 = 4/8, which is correct, is obtained by
  cancelling the 9s.

  We shall consider fractions like, 30/50 = 3/5, to be trivial
  examples.

  There are exactly four non-trivial examples of this type of
  fraction, less than one in value, and containing two digits in
  the numerator and denominator.

  If the product of these four fractions is given in its lowest
  common terms, find the value of the denominator."
  []
  (letfn [(curious-fraction [numer denom]
            (let [[n1 n2] (vec (digits numer))
                  [d1 d2] (vec (digits denom))
                  fraction (/ numer denom)]
              (or (and (= n1 d1) (not (= d2 0)) (= (/ n2 d2) fraction))
                  (and (= n1 d2) (= (/ n2 d1) fraction))
                  (and (= n2 d1) (not (= d2 0)) (= (/ n1 d2) fraction))
                  (and (not (= n2 0)) (= n2 d2) (= (/ n1 d1) fraction)))))]
    (denominator
      (apply * (for [numer (range 11 100)
                     denom (range (inc numer) 100)
                     :when (curious-fraction numer denom)]
                 (/ numer denom))))))

(defn problem-32
  "Pandigital products"
  []
  (letfn [(pandigital-products [coll]
            (for [a (range 1 9)
                  b (range a 9)
                  :let [[m1-digits m2-p] (split-at a coll)
                        [m2-digits product-digits] (split-at (- b a) m2-p)
                        m1 (digits->number m1-digits)
                        m2 (digits->number m2-digits)
                        p (digits->number product-digits)]
                  :when (= p (* m1 m2))]
              p))]
    (apply +
      (distinct
        (mapcat pandigital-products
                (comb/permutations (range 1 10)))))))

(defn problem-41
  "Largest pandigital prime

  This prime p cannot contain 9, as (= 45 (apply + (range 1 10)))
  which is divisible by 3. Similarly, p cannot contain 8, either."
  []
  (apply max (for [p (comb/permutations (range 1 8))
                   :while p
                   :let [n (digits->number p)]
                   :when (prime? n)]
               n)))

(defn problem-53
  "How many, not necessarily distinct, values of nCr, for
  (<= 1 n 100), are greater than one-million?"
  []
  (count (for [n (range 1 101)
               r (range 0 (inc n))
               :let [c (C n r)]
               :when (> c 1000000)
               ]
           c)))

(defn problem-56
  "Considering natural numbers of the form, a^b, where a, b < 100,
  what is the maximum digital sum?"
  []
  (apply max (for [a (range 2 100)
                   b (range 2 100)
                   :let [s (apply + (digits (math/expt a b)))]]
               s)))

(defn problem-46
  "What is the smallest odd composite that cannot be written as
  the sum of a prime and twice a square? "
  []
  (letfn [(breakable? [n]
            (first (for [p (take-while #(< % n) primes)
                         :let [r (- n p)
                               s (/ r 2)
                               rt (integer-sqrt s)]
                         :when (and (= r (* 2 s))
                                    (= s (* rt rt)))]
                     [p rt])))]
    (first (for [n (iterate #(+ 2 %) 3)
                 :when (and (not (prime? n))
                            (not (breakable? n)))]
             n))))

(defn problem-55
  "How many Lychrel numbers are there below ten-thousand?"
  []
  (letfn [(palindrome-number? [n]
            (let [ds (digits n)]
              (= ds (reverse ds))))
          (producing-palindrome-number? [number]
            (loop [i 1
                   n number]
              (if (> i 50)
                false
                (let [n' (+' n (digits->number (reverse (digits n))))]
                  (if (palindrome-number? n')
                    true
                    (recur (inc i) n'))))))]
    (count (filter #(not (producing-palindrome-number? %)) (range 1 10000)))))

(defn problem-43
  "Sub-string divisibility"
  []
  (let [ps (take 7 primes)]
    (apply +' (for [p (drop 362880 (comb/permutations (range 10)))
                    :let [sub (map digits->number (partition 3 1 (rest p)))]
                    :when (every? identity (map #(= (rem %1 %2) 0) sub ps))]
                (digits->number p)))))

(defn problem-38
  "Pandigital multiples"
  []
  (letfn [(concatenated-multiples [n]
            (loop [i 0
                   ds #{}
                   m []]
              (if (or (= (count ds) 9) (> i 9))
                (digits->number (mapcat digits m))
                (let [new-i (inc i)
                      mult (* new-i n)
                      d (digits mult)
                      dst (set d)]
                  (if (or (dst 0)
                          (not (= (count dst) (count d)))
                          (some ds d))
                    nil
                    (recur new-i (set/union ds dst) (conj m mult)))))))]
    (apply max (for [n (range 2 10000)
                     :let [m (concatenated-multiples n)]
                     :when m]
                 m))))

(defn problem-50
  "Which prime, below one-million, can be written as the sum of
  the most consecutive primes?"
  []
  (let [ps (take 550 primes)]
    (first (for [l (range (count ps) 20 -1)
                 p-list (partition l 1 ps)
                 :let [s (apply + p-list)]
                 :when (and (< s 1000000)
                            (prime? s))]
             s))))

(defn problem-47
  "Find the first four consecutive integers to have four distinct
  prime factors. What is the first of these numbers?

  That number must be at least equal to (* 2 3 5 7), which is 210."
  []
  (first (for [q (partition 4 1 (iterate inc 210))
               :when (every? #(= 4 (count (prime-factors %))) q)
               ]
           q)))

(defn problem-49
  "The arithmetic sequence, 1487, 4817, 8147, in which each of
  the terms increases by 3330, is unusual in two ways: (i) each
  of the three terms are prime, and, (ii) each of the 4-digit
  numbers are permutations of one another.

  There are no arithmetic sequences made up of three 1-, 2-, or
  3-digit primes, exhibiting this property, but there is one
  other 4-digit increasing sequence.

  What 12-digit number do you form by concatenating the three
  terms in this sequence?"
  []
  (apply concat
         (for [p (take-while #(< % 10000) (drop-while #(< % 1000) primes))
               :let [pd (sort (digits p))
                     r (for [d (range 1 5000)
                             :let [b (+ p d)
                                   c (+ b d)
                                   bd (sort (digits b))
                                   cd (sort (digits c))]
                             :while (< c 10000)
                             :when (and (prime? b)
                                        (prime? c)
                                        (= pd bd cd))]
                         [p b c])]
               :when (seq r)]
           r)))

(defn problem-44
  "Find the pair of pentagonal numbers, Pj and Pk, for which
  their sum and difference are pentagonal and D = |Pk − Pj| is
  minimised; what is the value of D?"
  []
  (first
    (apply concat
           (for [n1 pentagonal-numbers
                 :let [ans (for [d (take-while #(< % n1) pentagonal-numbers)
                                 :let [n2 (- n1 d)]
                                 :when (and (pentagonal-number? n2)
                                            (pentagonal-number? (+ n1 n2)))]
                             [n1 n2])]
                 :when ans]
             ans))))

(defn problem-92
  "A number chain is created by continuously adding the square
  of the digits in a number to form a new number until it has
  been seen before. How many starting numbers below ten million
  will arrive at 89?"
  []
  (loop [ends-in-89 #{89}
         result-count 0
         n 2
         current n
         to-add-to-cache [2]]
    (cond
      (>= n 10000000) result-count
      (= 1 current) (let [incremented (inc n)]
                      (recur ends-in-89
                             result-count
                             incremented
                             incremented
                             [incremented]))
      (ends-in-89 current) (let [incremented (inc n)]
                             (recur (into ends-in-89 to-add-to-cache)
                                    (inc result-count)
                                    incremented
                                    incremented
                                    [incremented]))
      :else (let [next-num (apply + (map #(* %1 %1) (digits current)))]
              (recur ends-in-89
                     result-count
                     n
                     next-num
                     (if (< next-num 1000)
                       (conj to-add-to-cache next-num)
                       to-add-to-cache))))))
