
(defclass expression-statement (statement)
  ((kind :initarg :kind :initform :expression-statement :reader kind)
   (expression :initarg :expression :initform (error "Must supply a value for expression") :reader expression)))

(defmethod initialize-instance :after ((object expression-statement) &key)
  (with-slots (expression) object
    (setf expression (parse-simple-sexp (expression object)))))
