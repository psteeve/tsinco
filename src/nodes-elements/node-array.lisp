(defclass node-array (node text-range)
  ((has-trailing-comma :initarg :has-trailing-comma :reader has-trailing-comma?)
   (node-list :initarg :node-list :initform (error "Must supply a value node-list") :reader node-list)))

(defmethod initialize-instance :after ((object node-array) &key)
  (with-slots (node-list) object
    (setf node-list (mapcar #'parse-simple-sexp (node-list object)))))
