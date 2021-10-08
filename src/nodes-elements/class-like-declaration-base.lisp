(defclass class-like-declaration-base (named-declaration)
  ((kind :initarg :kind
         :initform :class-like-declaration-base
         :reader kind)
   (name :initarg :name
         :reader name)
   (type-parameters :initarg :type-parameters
                    :reader type-parameters)
   (heritage-clauses :initarg :heritage-clauses
                     :reader heritage-clauses)
   (members :initarg :members
            :initform (error "Must supply a value for \"members\"")
            :reader members)))
