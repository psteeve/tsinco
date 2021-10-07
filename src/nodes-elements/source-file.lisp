(defclass source-file (a-declaration)
  ((kind :initarg :kind
         :initform :source-file
         :reader kind)
   (statements :initarg :statements
               :initform (error "Must supply a value for statements")
               :reader statements)
   (end-of-file-token :initarg :end-of-file-token
                      :reader end-of-file-token)
   (filename :initarg :filename
             :initform (error "Must supply a filename")
             :reader filename)
   (text :initarg :text
         :initform (error "Must supply a value for text")
         :reader text)
   (external-module-indicator :initarg :external-module-indicator
                              :initform nil
                              :accessor external-module-indicator)
   (amd-dependencies :initarg :amd-dependencies
                     :initform nil
                     :accessor amd-dependencies)
   (module-name :initarg :module-name
                :accessor module-name)
   (referenced-files :initarg :referenced-files
                     :initform nil
                     :accessor referenced-files)
   (type-reference-directives :initarg :type-reference-directives
                              :initform nil
                              :accessor type-reference-directives)
   (lib-reference-directives :initarg :lib-reference-directives
                             :initform nil
                             :accessor lib-reference-directives)
   (language-variant :initarg :language-variant
                     :initform :standard
                     :reader language-variant)
   (is-declaration-file :initarg :is-declaration-file
                        :accessor is-declaration-file)
   (has-no-default-lib? :initarg :has-no-default-lib
                        :initform nil
                        :reader has-no-default-lib?)
   (language-version :initarg :language-version
                     :initform :es5
                     :reader language-version)
   (parse-diagnostics :initarg :parse-diagnostics
                      :reader parse-diagnostics)
   
   (node-count :initarg :node-count
               :reader node-count)
   (pragmas :initarg :pragmas
            :reader pragmas)
   (bind-diagnostics :initarg :bind-diagnostics
                     :reader bind-diagnostics)
   (identifiers :initarg :identifiers
                :reader identifiers)
   (identifier-count :initarg :identifier-count
                     :reader identifier-count)
   (script-kind :initarg :script-kind
                :reader script-kind)
   (transform-flags :initarg :transform-flags
                    :reader transform-flags)
   (ambient-module-names :initarg :ambient-module-names
                         :reader ambient-module-names)
   (module-augmentations :initarg :module-augmentations
                         :reader module-augmentations)
   (imports :initarg :imports
            :reader imports)
   (original-file-name :initarg :original-file-name
                       :reader original-file-name)
   (resolved-path :initarg :resolved-path
                  :reader resolved-path)
   (path :initarg :path
         :reader path)
   (modifier-flags-cache :initarg :modifier-flags-cache
                         :reader modifier-flags-cache)))

(defmethod initialize-instance :after ((object source-file) &key)
  (with-slots (statements end-of-file-token) object
    (setf statements (mapcar #'parse-simple-sexp (statements object)))
    (setf end-of-file-token (parse-simple-sexp (end-of-file-token object)))))
