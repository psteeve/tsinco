(in-package :tsinco.utils)

(defun make-instance-for (object-name value)
  (apply #'make-instance object-name value))

(defun parse-simple-sexp (sexp)
  "Parse an expression of the form (:tag :prop 1 :prop2 2 :prop3 3)
to produce an object of type tag
example: (parse-simple-sexp '(:identifier :pos 20 :end 34 :escaped-text \"log\"))
return an object identifier with props (:pos 20 :end 34 :escaped-text \"log\")
assume the definition of identifier exists."
  (destructuring-bind (tag &rest props) sexp
    (let ((object-name (find-symbol (symbol-name tag))))
      (make-instance-for object-name props))))
