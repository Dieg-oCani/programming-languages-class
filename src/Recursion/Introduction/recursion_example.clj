(defn !
  "This function returns the factorial of a number n. Where n > 1."
  [n]
  (if (zero? n)
    1
    (*' n (! (dec n))))) ;; The ' after the * allows to create large integers.


(defn !-loop
  "Computes factorial of a number using loop/recur."
  [n]
  (loop [index        1  ;; this is an index that will accumulate.
         result   1]   ;; this is where we will save our computation.
    (if (> index n)   result   ;; when this is true, it will return result.
                      (recur (inc index) (*' result index)))))   ;; This is how we change the values of index and result.


(defn dup
  "Returns a list with a duplicate of every element of a list. Explicit recursion."
  [lst]
  (if (empty? lst)
    ()
    (cons (first lst)
          (cons (first lst)
                (dup (rest lst))))))

(defn dup-loop
  "Returns a list with a duplicate of every element in a list using loop/recur."
  [lst]
  (loop [lst lst
         result ()]
    (if (empty? lst)   (reverse result)
                       (recur (rest lst)
                              (cons (first lst)
                                    (cons (first lst) result))))))





(defn pow
  "Returns the power of a number, (pow base exponent)"
  [n m]
  (if(zero? m)
    1
    (*' n (pow n (dec m)))))

(defn countdown
  "Counts down from a number n to zero."
  [n]
  (if (zero? n)
    ()
    (cons n (countdown (dec n)))))

(defn log2
  "Returns the log base 2 of  given number, rounded to the closest number."
  [n]
  (if (= n 1)
    0
    (inc (log2 (quot n 2)))))


(defn howmany
  "Returns the number of ocurrences of a given number in a given list.
  Supports nested lists."
  [x lst]
  (cond
    (empty? lst)
    ;; if the list is empty, return a zero.
    0
    (list? (first lst)) ;; if the first element of the list is another lis, recall function.
    ( + (howmany x (first lst)) ;; divide both recursive calls and add them together.
        (howmany x (rest lst)))
    (= x (first lst))
    (inc (howmany x (rest lst)))
    :else
    (howmany x (rest lst))))



(defn fibo
  "Returns the n-th element of the Fibonacci sequence. Using explicit recursion."
  [n]
  (if (< n 2)   n
                (+ (fibo (- n 1)) (fibo (- n 2)))))

(defn fibo-loop
  "Returns the n-th element of the Fibonacci sequence. Using loop/recur."
  [n]
  (loop [a 0  ;; to store the first two elements.
         b 1
         index 0]
    (if (>= index n) a
                    (recur b (+' a b) (inc index)))))




