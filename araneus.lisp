;; This is a minimal web-framework built on ningle that attempts to decouple
;; views from controllers and make life more interesting.
(in-package :araneus)

(defgeneric controller (name params &key))
(defgeneric view (name model))
(defgeneric run-route (name params &rest r))

(defmacro setf1 (&body body)
  "Make setf a bit nicer"
  (list* 'setf (apply #'append body)))

(defmacro defroutes (app &body routes)
  (alexandria:once-only (app)
    (list* 'setf1
           (loop for ((target &key method) callback) in routes
                 collect `((ningle:route ,app ,target :method ,(or method :GET)) ,callback)))))

 
(defmacro as-route (name &rest r &key &allow-other-keys)
  `(lambda (params) (run-route ,name params ,@r)))


(defmethod run-route (name params &rest r)
  (view name (apply #'controller (list* name params r))))

(defmethod controller (name params &key)
  params)

(defmacro define-controller (name (params &rest r &key &allow-other-keys) &body body)
  `(defmethod controller ((name (eql ',name)) ,params &key ,@r)
     ,@body))

(defmacro define-view (name (model) &body body)
  `(defmethod view ((name (eql ',name)) ,model)
     ,@body))

(defun render-mustache (fn data)
  (with-open-file (s (truename fn))
    (let ((template (make-string (file-length s))))
      (read-sequence template s)
      (mustache:render* template data))))


(defmacro mustache ((template lambda-list data) &body body)
  "Template specifies the template to be render, lambda-list is used to destructure data
   and body transforms the destructured data into an alist for use in the template"
  (alexandria:once-only (template)
    `(destructuring-bind ,lambda-list ,data
       (render-mustache ,template
                        (list
                          ,@(loop for (k v) on body by #'cddr
                                  if (or k v)
                                  collect `(cons ,k ,v)))))))

(defmacro mustache-view (name lambda-list template &body body)
  (alexandria:with-gensyms (model)
    `(define-view ,name (,model)
       (mustache (,template ,lambda-list ,model)
         ,@body))))

