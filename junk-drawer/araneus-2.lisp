;; This is a minimal web-framework built on ningle that attempts to decouple
;; views from controllers and make life more interesting.
(in-package :araneus-2)

(defgeneric run-route (name params &rest r)
  (:documentation "specialized on NAME with an EQL-specializer. This generic
                   function defines the way a specific route is to be processed"))

(defgeneric controller (route action &key)
  (:documentation "specialized on ROUTE to indicate which route is involved, ACTION to pick the action.

This generic function picks out the model that the view renders for the user. Normally, this
is specialized using the DEFINE-CONTROLLER macro. A route can specify additional"))

(defgeneric view (name model)
  (:documentation "specialized on NAME with an EQL-specializer. This generic function
                   renders the model picked out by the controller. Normally, this is
                   specialized using the DEFINE-VIEW macr"))

(defmacro defroutes (app &body routes)
  "Define a set of routes for given paths. the ROUTES parameter expects this format:
   ((\"/path/to/{route}\" :method :POST) route-callback) the AS-ROUTE macro helps one
   avoid binding function values to the route for flexibility."
  (alexandria:once-only (app)
    `(setfs
       ,@(loop for ((target &key method) callback) in routes
               collect `((ningle:route ,app ,target :method ,(or method :GET)) ,callback)))))


(defvar *current-route*)
(defun call-current-view (model)
  "Call the currently running view with a new model.
   
   Useful if one view name is specialized many times on different model classes: the controller can
   pass the container and then the view can recall itself on the contained items."
  (view *current-route* model))

;; DEPRECATED?
(defmacro as-route (route action &rest r &key &allow-other-keys)
  "Create a lambda directing requests to the route for NAME.  This uses the
   generic function RUN-ROUTE internally whose default implementation relies on
   appropriate implementations of CONTROLLER and VIEW. The RUN-ROUTE method receives
   the parameters ningle passes to other functions as a first parameter, and then it
   receives a bunch of arguments according to the arguments passed to this macro."
  `(lambda (params)
     (run-route ,route ,action ,@r)))

(defun %compose-route (controller controller-args view)
  (declare (optimize (debug 3) (speed 0) (space 0) (safety 3)))
  (lambda (params)
    (declare (optimize (debug 3) (speed 0) (space 0) (safety 3)))
    (let ((*current-route* view))
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

(defmethod run-route (route action &rest r)
  (let ((*current-route* route))
    (fw.lu:let-each (:be *)
      (list* route action r)
      (restart-bind ((switch-view (lambda (new-view)
                                    (format t "~%SWITCHING VIEW: ~a" new-view)
                                    (setf *current-route* new-view))))
        (apply #'controller *))
      (view *current-route* *))))

; The default controller just passes its parameters directly to the view
(defmethod controller (route action &key)
  action)

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

