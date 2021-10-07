(defclass named-imports (node)
  ((kind :initarg :kind
         :initform :named-imports
         :reader kind)
   (parent :initarg :parent
           :reader parent)
   (elements :initarg :elements
             :initform (error "Must supply a value for \"elements\""))))
