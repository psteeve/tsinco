(defclass return-statement (statement)
  ((kind :initarg :kind :initform :return-statement :reader kind)
   (expression :initarg :expression :reader expression)))

(defmethod initialize-instance :after ((object return-statement) &key)
  (with-slots (expression) object
    (if expression
        (setf expression (parse-simple-sexp (expression object))))))
