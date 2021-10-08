(defclass object-literal-element (named-declaration)
  ((object-literal-brand :initarg :object-literal-brand
                         :reader object-literal-brand)
   (name :initarg :name
         :reader name)))

