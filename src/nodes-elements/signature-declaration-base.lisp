
(defclass signature-declaration-base (named-declaration)
  ((kind :initarg :kind :initform :signature-declaration-base :reader kind)
   (name :initarg :name :reader name)
   (type-parameters :initarg :type-parameters :initform nil :reader type-parameters)
   (parameters :initarg :parameters :initform (error "Must supply a value for \"parameters\"") :reader parameters)
   (sig-type :initarg :sig-type :reader sig-type)))
