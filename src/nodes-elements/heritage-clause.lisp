(in-package :tsinco.node-elements)

(defclass heritage-clause (node)
  ((kind :initarg :kind
         :initform :heritage-clause
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must supply a value for \"parent\"")
           :reader parent)
   (token :initarg :token
          :initform (error "Must supply a value for \"token\"")
          :reader token)
   (types :initarg :types
          :initform (error "Must supply a value for \"types\"")
          :reader types)))
