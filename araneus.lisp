;; This is a minimal web-framework built on ningle that attempts to decouple
;; views from controllers and make life more interesting.
(in-package :araneus)

(defclass mixin ()
  ())

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

(define-condition view-error (error)
  ((%message :initarg :msg :reader msg))
  (:default-initargs :msg "something's wrong")
  (:report (lambda (condition stream)
             (format stream "Error in view: ~a" (msg condition)))))

(defgeneric styles (route)
  (:documentation "if you use class-based roues, this method provides
  a hook for generating CSS files with the most specific selectors
  last: it returns a list via the APPEND method complication, suitable
  for use with a system like lass <https://shinmera.github.io/LASS>")
  (:method-combination append :most-specific-last))

(defgeneric routes (app)
  (:documentation "defines routes for some object that represents an
  app: PROGN method combination, with an around method that ensures
  that this returns the app.")
  (:method-combination progn)
  (:method :around (app)
    (call-next-method)
    app))
(defmethod initialize-instance :after ((instance mixin) &key)
  (araneus:routes instance))

(defmacro defroutes (app &body routes)
  "Define a set of routes for given paths. the ROUTES parameter expects this format:
   ((\"/path/to/{route}\" :method :POST) route-callback) the AS-ROUTE macro helps one
   avoid binding function values to the route for flexibility."
  (alexandria:once-only (app)
    `(setf ,@(loop for ((target &key method) callback) in routes
                   append `((ningle:route ,app ,target :method ,(or method :GET)) ,callback)))))


(defvar *view-name*)
(defun call-current-view (model)
  "Call the currently running view with a new model.

   Useful if one view name is specialized many times on different model classes: the controller can
   pass the container and then the view can recall itself on the contained items."
  (if (boundp '*view-name*)
      (view *view-name* model)
      (error 'view-error :msg "Must call CALL-CURRENT-VIEW within a view")))

(defmacro as-route (name &rest r &key &allow-other-keys)
  "Create a lambda directing requests to the route for NAME.  This uses the
   generic function RUN-ROUTE internally whose default implementation relies on
   appropriate implementations of CONTROLLER and VIEW. The RUN-ROUTE method receives
   the parameters ningle passes to other functions as a first parameter, and then it
   receives a bunch of arguments according to the arguments passed to this macro."
  (alexandria:once-only (name)
    `(lambda (params)
       (run-route ,name params ,@r))))

(defun %compose-route (controller controller-args view)
  (declare (optimize (debug 3) (speed 0) (space 0) (safety 3)))
  (lambda (params)
    (declare (optimize (debug 3) (speed 0) (space 0) (safety 3)))
    (let ((*view-name* view))
      (apply #'view
	     (list view
		   (apply #'controller
			  (list* controller
				 params
				 controller-args)))))))

(defmacro compose-route ((controller &rest controller-args) view)
  `(%compose-route ',controller ,controller-args ',view))

(defun switch-view (view-name)
  (format t "~&Switching view to: ~a~&" view-name)
  (alexandria:if-let ((switch-view-restart (find-restart 'switch-view)))
    (invoke-restart switch-view-restart view-name)
    (cerror "ignore this error"
            "Can only call switch-view while the route is being processed")))

(defmethod run-route (name params &rest r)
  (let ((*view-name* name))
    (let* ((* (list* name params r))
           (*
             (restart-bind
                 ((switch-view
                    (lambda (new-view)
                      (format t "~%SWITCHING VIEW: ~a" new-view)
                      (setf *view-name* new-view))))
               (apply #'controller *))))
      (let ((* (funcall 'view *view-name* *)))
        *))))

; The default controller just passes its parameters directly to the view
(defmethod controller (name params &key)
  params)

(defmacro define-controller (name (params &rest r) &body body)
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


(defmacro define-spinneret-view (name (model) &body body)
  (let* ((declarations (when (eq (caar body) 'declare) (car body)))
         (body (if declarations (cdr body) body)))
    `(define-view ,name (,model)
       ,declarations
       (spinneret:with-html-string
         ,@body))))

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
