
(defclass binary-expression (expression a-declaration)
  ((kind :initarg :kind :initform :binary-expression :reader kind)
   (left :initarg :left :initform (error "Must supply a value for left") :reader left)
   (operator-token :initarg :operator-token
                   :initform (error "Must suppla a value for \"operator-token\"")
                   :reader operator-token)
   (right :initarg :right :initform (error "Must supply a value for right") :reader right)))

(defmethod initialize-instance :after ((object binary-expression) &key)
  (with-slots (left right operator-token) object
    (setf left (parse-simple-sexp (left object)))
    (setf operator-token (parse-simple-sexp (operator-token object)))
    (setf right (parse-simple-sexp (right object)))))
