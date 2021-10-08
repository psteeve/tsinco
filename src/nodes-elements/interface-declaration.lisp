(defclass interface-declaration (declaration-statement)
  ((kind :initarg :kind
         :initform :interface-declaration
         :reader kind)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\"")
         :reader name)
   (type-parameters :initarg :type-parameters
                    :reader type-parameters)
   (heritage-clauses :initarg :heritage-clauses
                     :reader heritage-clauses)
   (members :initarg :members
            :initform (error "Must supply a value for \"members\"")
            :reader members)))
