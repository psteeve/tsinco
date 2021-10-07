(defclass property-assignment (object-literal-element)
  ((parent :initarg :parent :reader parent)
   (kind :initarg :property-assignment :reader kind)
   (name :initarg :name :reader name)
   (question-token-? :initarg :question-token-? :reader question-token-?)
   (initializer :initarg :initializer :reader initializer)))

(defmethod initialize-instance :after ((object property-assignment) &key)
  (with-slots (name initializer) object
    (setf name (parse-simple-sexp name))
    (setf initializer (parse-simple-sexp initializer))))
