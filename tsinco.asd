(asdf:defsystem :tsinco
  :serial t
  :components ((:file "src/syntaxkind")
               (:file "src/ast")
               (:file "src/token"))
  :depends-on (:st-utils :flot))
