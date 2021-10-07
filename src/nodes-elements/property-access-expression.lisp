
(defclass property-access-expression (member-expression named-declaration)
  ((kind :initarg :kind :initform :property-access-expression :reader kind)
   (expression :initarg :expression :initform (error "Must supply an expression value") :reader expression)
   (name :initarg :name :initform (error "Must supplly a name value") :reader name)))

(defmethod initialize-instance :after ((object property-access-expression) &key)
  (with-slots (name expression) object
    (setf name (parse-simple-sexp (name object)))
    (setf expression (parse-simple-sexp (expression object)))))
