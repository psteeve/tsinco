(defclass import-clause (named-declaration)
  ((kind :initarg :kind
         :initform :import-clause
         :reader kind)
   (parent :initarg :parent
           :reader parent)
   (name :initarg :name
         :reader name)
   (is-type-only :initarg :is-type-only :reader is-type-only)
   (named-bindings :initarg :named-bindings
                   :reader named-bindings)))

(defmethod initialize-instance :after ((object import-clause) &key)
  (with-slots (named-bindings) object
    (setf named-bindings (parse-simple-sexp (named-bindings object)))))
