(defclass array-literal-expression (primary-expression)
  ((kind :initarg :kind :initform :array-literal-expression :reader kind)
   (elements :initarg :elements :initform (error "Must supply a value for elements.") :reader elements)
   (multi-line :initarg :multi-line :reader multi-line?)))
