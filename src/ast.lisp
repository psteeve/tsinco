(in-package :tsinco.ast)

(defgeneric get-compiler-options (script-reference-host))
(defgeneric get-source-file (script-reference-host file-name))
(defgeneric get-source-file-by-path (script-reference-host path))
(defgeneric get-current-directory (script-reference-host))

(defgeneric get-root-file-names (program)
  (:documentation "Get a list of root file names that where passed to a \"createProgram\""))

(defgeneric get-source-files (program)
  (:documentation "Get a list of files in the program."))

(defgeneric emit (program &key
                            target-source-file?
                            write-file?
                            cancellation-token?
                            emit-only-dts-files?
                            custom-transformers?
                            ))

(defgeneric get-options-diagnostics (program cancellation-token?))
(defgeneric get-global-diagnostics (program cancellation-token?))
(defgeneric get-syntactic-diagnostics (program &key
                                                 source-file?
                                                 cancellation-token?))

(defgeneric get-semantic-diagnostics (program &key
                                                source-file?
                                                cancellation-token?))

(defgeneric get-declaration-diagnostics (program &key
                                                   source-file?
                                                   cancellation-token?))

(defgeneric get-config-file-parsing-diagnostics (program))

(defgeneric get-type-checker (program))

(defgeneric is-source-file-from-external-library (program))
(defgeneric is-source-file-from-default-library (program))
(defgeneric get-project-references (program))
(defgeneric get-resolved-project-references (program))
