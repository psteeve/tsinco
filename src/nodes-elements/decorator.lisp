(in-package :tsinco.node-elements)

(defclass decorator (node)
  ((kind :initarg :kind
         :initform :decorator
         :reader kind)
   (expression :initarg :expression
               :initform (error "Must provide a value for \"expression\"")
               :reader expression)))

(defmethod initialize-instance :after ((object decorator) &key)
  (with-slots (expression) object
    (setf expression (parse-simple-sexp expression))))
