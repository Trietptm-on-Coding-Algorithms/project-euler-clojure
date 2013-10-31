(require '[clojure.java.io :as io]
         '[clojure.string :as str])

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
  (butlast (divisors n)))

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
    (apply compare-triplets
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
    (apply compare-pairs [0 0] (map #(vector % (recurring-cycle-length %))
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
  For which value of p ≤ 1000, is the number of solutions maximised?
  "
  []
  (letfn [(compare-pairs [[_ c1 :as p1] [_ c2 :as p2]]
            (if (> c1 c2) p1 p2))]
    (apply compare-pairs
      (map #(vector (first %) (count %))
           (partition-by identity
                         (sort  (for [a (range 1 1000)
                                      b (range a 1000)
                                      :let [c2 (+ (* a a) (* b b))
                                            c (integer-sqrt c2)
                                            p (+ a b c)]
                                      :when (and (= c2 (* c c))
                                                 (<= p 1000))]
                                  p)))))))

(defn problem-33
  " The fraction 49/98 is a curious fraction, as an inexperienced
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
      (apply * (map #(/ (% 0) (% 1))
                    (filter #(apply curious-fraction %)
                            (for [numer (range 11 100)
                                  denom (range (inc numer) 100)]
                              [numer denom])))))))
