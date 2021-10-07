(defclass enum-member (named-declaration)
  ((kind :initarg :kind :initform :enum-member :reader kind)
   (name :initarg :name :initform (error "Must supply a value for \"name\"") :reader name)
   (initializer :initarg :initializer :reader initializer)))
