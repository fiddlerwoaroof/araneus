;; This is a minimal web-framework built on ningle that attempts to decouple
;; views from controllers and make life more interesting.
(in-package :araneus)

(defgeneric run-route (name params &rest r)
  (:documentation "specialized on NAME with an EQL-specializer. This generic
                   function defines the way a specific route is to be processed"))

(defgeneric controller (name params &key)
  (:documentation "specialized on NAME with an EQL-specializer. This generic function
                   picks out the model that the view renders for the user. Normally,
                   this is specialized using the DEFINE-CONTROLLER macro."))

(defgeneric view (name model)
  (:documentation "specialized on NAME with an EQL-specializer. This generic function
                   renders the model picked out by the controller. Normally, this is 
                   specialized using the DEFINE-VIEW macr"))

(defmacro setf1 (&body body)
  "Make setf a bit nicer to use with paredit"
  (list* 'setf (apply #'append body)))

(defmacro defroutes (app &body routes)
  "Define a set of routes for given paths. the ROUTES parameter expects this format:
   ((\"/path/to/{route}\" :method :POST) route-callback) the AS-ROUTE macro helps one
   avoid binding function values to the route for flexibility."
  (alexandria:once-only (app)
    (list* 'setf1
           (loop for ((target &key method) callback) in routes
                 collect `((ningle:route ,app ,target :method ,(or method :GET)) ,callback)))))

 
(defmacro as-route (name &rest r &key &allow-other-keys)
  "Create a lambda directing requests to the route for NAME.  This uses the
   generic function RUN-ROUTE internally whose default implementation relies on
   appropriate implementations of CONTROLLER and VIEW. The RUN-ROUTE method receives
   the parameters ningle passes to other functions as a first parameter, and then it
   receives a bunch of arguments according to the arguments passed to this macro."
  `(lambda (params) (run-route ,name params ,@r)))


(defmethod run-route (name params &rest r)
  (view name (apply #'controller (list* name params r))))

; The default controller just passes its parameters directly to the view
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
  "Define a view that renders a mustache template."
  (alexandria:once-only (name lambda-list)
    (alexandria:with-gensyms (model)
      `(define-view ,name (,model)
         (mustache (,template ,lambda-list ,model)
                   ,@body)))))

