(defn runtime []
  (System/currentTimeMillis))

(defn square [x] (* x x))

(defn divides? [a b] (= (rem b a) 0))

(defn find-divisor [n test-divisor]
  (cond
    (> (square test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n] (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn report-prime [n elapsed-time]
  (print n)
  (print " is prime! *** ")
  (print elapsed-time)
  (println " ms"))

(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime n (- (runtime) start-time))))

(defn timed-prime-test [n]
  (start-prime-test n (runtime)))

(defn search-for-primes [from to]
  (timed-prime-test from)
  (if (< from to) (search-for-primes (+ from 1) to)))


(search-for-primes 1000 1200)