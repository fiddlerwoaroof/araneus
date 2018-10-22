(in-package :araneus.routes)

(defgeneric resolve-route (url method content-type &key headers)
  (:documentation "The most generic method for resolving a route.
                   
                   Specialize with a keyword eql-specializer for method or content-type
                   to define a particular understanding of either"))

(defgeneric match-token (url token)
  (:documentation "Match a url against a given token."))

(defclass match-tree ()
  ((-token :initarg :token :accessor token)
   (-children :accessor children :initform (make-array 5 :adjustable t :fill-pointer 0))
   (-name :initarg :name :accessor name :initform nil)))



(defun token= (tok1 tok2)
  (if (typep tok1 'match-tree)
    (setf tok1 (token tok1)))

  (if (typep tok2 'match-tree)
    (setf tok2 (token tok2)))

  (equal tok1 tok2))

(defgeneric add-to-tree (match-tree token)
  (:method ((match-tree match-tree) (tokens list))
    (loop with current = match-tree
          for token in tokens
          do (setf current (add-to-tree current token))))

  (:method ((match-tree match-tree) (token integer))
   (with-accessors ((children children)) match-tree
     (if (eql (token match-tree) token)
       match-tree
       (prog1 (elt children
                   (or (position token (children match-tree) :test #'token=)
                       (vector-push-extend (make-instance 'match-tree :token token)
                                           children)))
         (setf children (stable-sort children #'< :key #'token)))))))

(define-modify-macro orf (it) or)

(defgeneric match-match-tree (match-tree item)
  (:method ((match-tree match-tree) (tokens list))
    (let ((current match-tree))
      (dolist (token tokens current)
        (let ((next (match-match-tree current token)))
          (setf current next)))
      ))
  (:method ((match-tree match-tree) (token integer))
    (find token
          (children match-tree)
          :test #'token=)))

(defun random-match-tree (&optional (iters 10))
  (flet ((random-list (len lim) (loop repeat len collect (1+ (random lim)))))
    (let ((result (make-instance 'match-tree :token nil)))
      (loop repeat iters
            do (add-to-tree result
                            (random-list (1+ (random 5)) (1+ (random 10))))
            finally (return result)))))

(defun print-match-tree (match-tree &optional (level 0))
  (format t "~&~v,2,1,'-<~>~a~%" level (token match-tree))
  (loop for child across (children match-tree)
        do (print-match-tree child (1+ level))))

(coalesce-lists
  (coalesce-lists '(1 2 3) '(1 2 3 4))
  (coalesce-lists '(1 2 4) '(1 3 4)))
; => '(1 (2 3) (3 4))

(defun match-url)
