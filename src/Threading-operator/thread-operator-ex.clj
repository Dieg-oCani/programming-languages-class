

(defn binary
  "Returns a list of binary digits representing the converted value of n."
  [n]
  (->>
    [n ()]
    (iterate (fn [[n lst]]
                 [(quot n 2) (cons (rem n 2) lst)]))
    (drop-while (fn [[n lst]]  (not= n 0)))
    first
    second))




