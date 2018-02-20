
;; use is used like import in other languages.
(use 'clojure.test)
(use 'clojure.math.numeric-tower)


(defn f2c
  "Converts from farenheit to celsius degrees."
  [f]
  (/ (* (- f 32) 5) 9))


(defn sign
  "Checks if a number is negative (returns -1), zero (returns 0) or positive (returns 1)"
  [n]
  (if (< n 0) ;; if n is less than zero, return a -1
    -1
    (if (> n 0) ;; if n is greater than zero return 1
      1
      0))) ;; in any other case, return 0


(defn roots
  "Computes the roots of a given quadratic equiation with coefficients x, y and z."
  [a b c]
  (let [d (- b)
        e (sqrt (- (* b b) (*  4 a c)))
        f (* 2 a)
        x1 (/ (+ d e) f)
        x2 (/ (- d e) f)]
    [x1 x2]))





;; we declare tests automatically, and the results will appear in the REPL.

;; tests for f2c
(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

;; tests for sign
(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))


;; run the tests.
(run-tests)

