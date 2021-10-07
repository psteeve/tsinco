(defclass qualified-name (node)
  ((kind :initarg :kind
         :initform :qualified-name
         :reader kind)
   (left :initarg :left
         :initform (error "Must supply a value for \"left\"")
         :reader left)
   (right :initarg :right
          :initform (error "Must supply a value for \"right\"")
          :reader right)))
