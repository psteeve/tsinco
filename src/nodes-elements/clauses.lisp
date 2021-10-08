(in-package :tsinco.node-elements)


(defclass cach-clause (node)
  ((kind :initarg :kind :initform :cach-clause :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent.") :reader parent)
   (variable-declaration :initarg :variable-declaration :reader variable-declaration)
   (a-block :initarg :a-block :initform (error "Must supply a value for a-block.") :reader a-block)))

(defclass case-block (node)
  ((kind :initarg :kind
         :initform :case-block
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must provide a value for \"parent\"")
           :reader parent)
   (clauses :initarg :clauses
            :initform (error "Must provide a value for \"clauses\"")
            :reader clauses)))

(defclass case-clause (node)
  ((kind :initarg :kind
         :initform :case-clause
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must provide a value for \"parent\"")
           :reader parent)
   (expression :initarg :expression
               :initform (error "Must provide a value for \"expression\"")
               :reader expression)
   (statements :initarg :statements
               :initform (error "Must provide a value for \"statements\"")
               :reader statements
               )))

(defclass default-clause (node)
  ((kind :initarg :kind
         :initform :default-clause
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must provide a value for \"parent\"")
           :reader parent)
   (statements :initarg :statements
               :initform (error "Must provide a value for \"statements\"")
               :reader statements)))
