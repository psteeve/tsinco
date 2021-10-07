(defclass function-declaration (function-like-declaration-base declaration-statement)
  ((kind :initarg :kind :initform :function-declaration :reader function-declaration)
   (name :initarg :name :reader name)
   (body :initarg :body :reader body)))

(defmethod initialize-instance :after ((object function-declaration) &key)
  (with-slots (name body parameters modifiers) object
    (setf parameters (mapcar #'parse-simple-sexp (parameters object)))
    (setf name (parse-simple-sexp (name object)))
    (setf body (parse-simple-sexp (body object)))
    (setf modifiers (mapcar #'parse-simple-sexp (modifiers object)))))
