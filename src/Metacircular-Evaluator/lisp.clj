;; Helper functions:
(defn third
  "This function returns the third element of a list"
  [lst]
  (nth lst 2))

(defn fourth
  "Returns the fourth element of a list."
  [lst]
  (nth lst 3))

;; Functions:
(defn evaluate
  "Evaluate an expression expression in the context of an environment (variables in scope) env and returns the result."
  [expr env]
  ;; Environment is like a table with the variables (bindings) and their values in the current scope.
  (cond
    ;; Variable references (bindings)
    (symbol? expr) (if (contains? env expr)
                     (get env expr)
                     (throw (RuntimeException. (str "Variable not found: " expr))))
    ;; Clojure's special forms. (clojure's functions are represented through lists)
    (list? expr) (case (first expr)
                   nil '()
                   quote (second expr)
                   if  (if (evaluate (second expr) env)
                         (evaluate (third expr) env)
                         (evaluate (fourth expr) env)))



    ;; Functions and function invocations.

    ;; Everything else (something that is not a symbol or a clojure function will be returned as it is.
    ;; this part considers only primitives like integers, strings and boolean values.
    :else
    expr))
