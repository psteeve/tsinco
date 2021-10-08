(defclass object-binding-pattern (node)
  ((kind :initarg :kind :initform :object-binding-pattern :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value form parent.") :reader parent)
   (elements :initarg :elements :initform (error "Must supply a value form elements.") :reader elements)))
