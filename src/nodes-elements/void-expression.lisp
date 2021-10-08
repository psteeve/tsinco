(defclass void-expression (unary-expression)
  ((kind :initarg :kind :initform :void-expression :reader kind)
   (expression :initarg :expression :initform (error "Must supply a value for expression.") :reader expression)))
