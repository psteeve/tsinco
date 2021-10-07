(defclass call-expression (left-hand-side-expression a-declaration) 
  ((kind :initarg :kind :initform :call-expression :reader kind)
   (expression :initarg :expression :initform (error "Must suplly a value for expression") :reader expression)
   (type-arguments :initarg :type-arguments :accessor type-arguments)
   (arguments :initarg :arguments :initform (error "Must supply a value for arguments") :reader arguments)))

(defmethod initialize-instance :after ((object call-expression) &key)
  (with-slots (expression arguments) object
    (setf expression (parse-simple-sexp (expression object)))
    (setf arguments (parse-simple-sexp (arguments object)))))
