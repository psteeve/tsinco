(defclass property-signature (type-element)
  ((kind :initarg :kind :initform :property-signature :reader kind)
   (name :initarg :name :initform (error "Must supply a value for name.") :reader name)
   (question-token :initarg :question-token :reader question-token)
   (initializer :initarg :initializer :reader initializer)))
