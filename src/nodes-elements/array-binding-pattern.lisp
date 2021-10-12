(defclass array-binding-pattern (node)
  ((kind :initarg :kind
         :initform :array-binding-pattern
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must supply a value for parent.")
           :reader parent)
   (elements :initarg :elements
             :initform (error "Must supply a value for elements.")
             :reader elements)))
