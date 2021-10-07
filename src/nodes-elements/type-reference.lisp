(defclass type-reference (node-with-type-arguments)
  ((kind :initarg :kind
         :initform :type-reference
         :reader kind)
   (type-name :initarg :type-name
              :initform (error "Must supply a value for \"type-name\"")
              :reader type-name)))

(defmethod initialize-instance :after ((object type-reference) &key)
  (with-slots (type-name type-arguments) object
    (setf type-name (parse-simple-sexp (type-name object)))))
