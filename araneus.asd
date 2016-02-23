;;;; araneus.asd

(asdf:defsystem #:araneus
  :description "Describe araneus here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:anaphora
               #:alexandria
               #:serapeum
               #:ningle
               #:cl-mustache)
  :serial t
  :components ((:file "package")
               (:file "araneus")))

;; vim: ft=lisp:
