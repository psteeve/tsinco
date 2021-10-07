(defclass module-block (statement)
  ((kind :initarg :kind
         :initform :module-block
         :reader kind)
   (statements :initarg :statements
               :reader statements
               :initform (error "Must supply a value for \"statements\""))))

(defmethod initialize-instance :after ((object module-block) &key)
  (with-slots (statements) object
    (setf statements (mapcar #'parse-simple-sexp (statements object)))))
