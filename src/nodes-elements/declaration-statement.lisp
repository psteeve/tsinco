(defclass declaration-statement (named-declaration statement)
  ((name :initarg :name :reader name)))
