(defclass parenthesized-expression (primary-expression)
  ((kind :initarg :kind :initform :parenthesized-expression :reader kind)
   (expression :initarg :expression :initform (error "Must supply a value for expression.") :reader expression)))
