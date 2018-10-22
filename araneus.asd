;;;; araneus.asd

(asdf:defsystem #:araneus
  :description "Another web framework"
  :author "Edward Langley <el@elangley.org>"
  :license "MIT"
  :depends-on (#:anaphora
               #:alexandria
               #:serapeum
               #:ningle
               #:fwoar.lisputils
               #:spinneret
               #:cl-mustache)
  :serial t
  :components ((:file "package")
               (:file "araneus")))

;; vim: ft=lisp:
