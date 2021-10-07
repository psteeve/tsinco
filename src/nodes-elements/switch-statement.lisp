(in-package :tsinco.node-elements)

(defclass switch-statement (statement)
  ((kind :initarg :kind :initform :switch-statement :reader kind)
   (expression :initarg :expression :initform (error "Must provide a value for \"expression\"") :reader expression)
   (case-block :initarg :case-block :initform (error "Must provide a value for \"case-block\"") :reader case-block)
   (possibly-exhaustive :initarg :possibly-exhaustive :reader possibly-exhaustive)))
