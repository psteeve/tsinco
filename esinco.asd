(asdf:defsystem :esinco
  :serial t
  :components ((:file "syntaxkind")
               (:file "ast")
               (:file "token"))
  :depends-on (:st-utils :flot))
