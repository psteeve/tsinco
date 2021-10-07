(defclass new-expression (primary-expression declaration)
  ((kind :initarg :kind
         :initform :new-expression
         :reader kind)
   (expression :initarg :expression
               :initform (error "Must supply a value for \"expression\"")
               :reader expression)
   (type-arguments :initarg :type-arguments
                   :reader type-arguments)
   (arguments :initarg :arguments
              :reader arguments)))
