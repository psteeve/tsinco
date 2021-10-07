
(defclass parameter (named-declaration)
  ((kind :initarg :kind :initform :parameter :reader kind)
   (parent :initarg :parent :reader parent)
   (name :initarg :name :initform (error "Must supply value for name") :reader name)
   exclamation-token
   (type-parameter :initarg :type-parameter :reader type-parameter)
   initializer))

(defmethod initialize-instance :after ((object parameter) &key)
  (with-slots (name type-parameter) object
    (setf name (parse-simple-sexp (name object)))
    (setf type-parameter (parse-simple-sexp (type-parameter object)))))
