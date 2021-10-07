(defclass module-declaration (declaration-statement)
  ((kind :initarg :kind
         :initform :module-declaration
         :reader kind)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\"")
         :reader name)
   (body :initarg :body
         :reader body)))

(defmethod initialize-instance :after ((object module-declaration) &key)
  (with-slots (body name) object
    ;;    (setf body (mapcar #'parse-simple-sexp (body object)))
    (setf name (mapcar #'parse-simple-sexp (name object)))))
