;;;; araneus-demo.asd

(asdf:defsystem #:araneus-demo
  :description "Describe araneus-demo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:serapeum
               #:araneus)
  :serial t
  :components ((:file "package")
               (:file "araneus-demo")))

