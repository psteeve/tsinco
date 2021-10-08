(defclass object-literal-expression-base (object-literal-element primary-expression named-declaration)
  ((properties :initarg :properties
               :initform (error "Must supply a value for \"properties\"")
               :reader properties)))

(defmethod initialize-instance :after ((object object-literal-expression-base) &key)
  (with-slots (properties) object
    (setf properties (mapcar #'parse-simple-sexp properties))))
