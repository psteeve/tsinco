(defclass a-block (statement)
  ((kind :initarg :kind :initform :a-block :reader kind)
   (multi-line :initarg :multi-line :reader multi-line)
   (statements :initarg :statements
               :initform (error "Must supply value for statements")
               :reader statements)))

(defmethod initialize-instance :after ((object a-block) &key)
  (with-slots (statements) object
    (setf statements (mapcar #'parse-simple-sexp (statements object)))))
