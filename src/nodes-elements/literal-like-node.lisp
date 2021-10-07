
(defclass literal-like-node (node)
  ((text :initarg :text :initform (error "Must supply a value") :reader text)
   (is-unterminated :initarg :is-untermnated :reader is-unterminated?)
   (has-extended-unicode-escape :initarg :has-extended-unicode-escape :reader has-extended-unicode-escape?)))
