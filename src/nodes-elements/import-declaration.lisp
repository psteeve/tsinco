(defclass import-declaration (statement)
  ((kind :initarg :kind
         :initform :import-declaration
         :reader kind)
   (import-clause :initarg :import-clause
                  :reader import-clause)
   (module-specifier :initarg :module-specifier
                     :initform (error "Must supply a value for \"module-specifier\"")
                     :reader module-specifier)))

(defmethod initialize-instance :after ((object import-declaration) &key)
  (with-slots (import-clause module-specifier) object
    (setf import-clause (parse-simple-sexp (import-clause object)))
    (setf module-specifier (parse-simple-sexp (module-specifier object)))))
