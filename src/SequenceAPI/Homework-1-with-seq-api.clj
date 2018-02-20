;; This exapmles are solved using the sequence API.


(defn insert
  "This function inserts a value x in it's correct position in a list."
  [x lst]
  (concat (concat (take-while #(< % x) lst) (list x)) (drop-while #(< % x) lst)))
;; take-while takes the first part of the search, and drop-while deletes the first part of the search
;; together they provide the list provided.

(defn binary
  "Returns a list of binary digits representing the converted value of n."
  [n]
  (second
    (first
      (drop-while (fn [[n lst]] (not= n 0))
        (iterate (fn [[n lst]] [(quot n 2) (cons (rem n 2) lst)])  [n ()])))))