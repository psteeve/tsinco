(defclass enum-declaration (declaration-statement)
  ((kind :initarg :kind :initform :enum-declaration :reader kind)
   (name :initarg :name :initform (error "Must supply a value for \"name\"") :reader name)
   (members :initarg :members :initform (error "Must supply a value for \"members\"") :reader members)))
