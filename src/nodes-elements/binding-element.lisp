(defclass binding-element (named-declaration)
  ((kind :initarg :kind :initform :binding-element :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent.") :reader parent)
   property-name
   dotdot-token
   (name :initarg :name :initform (error "Must supply a value for name") :reader name)
   initializer))
