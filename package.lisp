;;;; package.lisp

(defpackage #:araneus
  (:use #:cl #:fw.lu)
  (:export #:defroutes  #:as-route  #:define-controller #:define-view    
           #:controller #:view      #:run-route #:mustache-view    
           #:render-mustache        #:define-spinneret-view
           #:setf1      #:call-current-view))

(defpackage #:araneus.routes
  (:use #:cl #:alexandria #:fw.lu)
  (:export #:route))

(defpackage #:araneus.form
  (:use #:cl)
  (:export ))


