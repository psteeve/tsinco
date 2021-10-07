(in-package :tsinco.node-elements)

(defclass class-declaration (class-like-declaration-base declaration-statement)
  ((kind :initarg :kind
         :initform :class-declaration
         :reader kind)
   (name :initarg :name
         :reader name)))

(defmethod initialize-instance :after ((object class-declaration) &key)
  (with-slots (decorators name modifiers) object
    (setf decorators (mapcar #'parse-simple-sexp (decorators object)))
    (setf name (parse-simple-sexp (name object)))
    (setf modifiers (mapcar #'parse-simple-sexp (modifiers object)))))
