(defclass export-declaration (declaration-statement)
  ((kind :initarg :kind
         :initform :export-declaration
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must supply a value for \"parent\"")
           :reader parent)
   (import-clause :initarg :import-clause
                  :reader import-clause)
   (module-specifier :initarg :module-specifier
                     :reader module-specifier)))
