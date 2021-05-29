;;; -*- Lisp -*-

(in-package "CL-USER")

(defvar *continuation-position* :first
  "One of :first or :last. Indicating whether the introduced continuation
should be the leftmost, first argument or the rightmost, last argument.")

(defvar *add-funcalls* nil
  "Add funcall where needed for common lisp.")

(defun cps-convert (expression continuation)
  (expression-dispatch expression
    ;; case application
    (lambda (subexpressions)
      (labels ((l (subexpressions revevaled)
                 (if (null subexpressions)
                     (let ((evaled (reverse revevaled)))
                       (let ((operator (car evaled))
                             (operands (cdr evaled)))
                         (make-application
                          `(,@(when *add-funcalls*
                                '(funcall))
                              ,operator
                              ,@(when (eq *continuation-position* :first)
                                  (list continuation))
                              ,@operands
                              ,@(when (eq *continuation-position* :last)
                                  (list continuation))))))
                     (if (symbolp (car subexpressions))
                         (l (cdr subexpressions) (cons (car subexpressions) revevaled))
                         (let ((temp (gensym)))
                           (cps-convert (car subexpressions)
                                        (make-lambda (list temp)
                                                     (l (cdr subexpressions)
                                                        (cons temp revevaled)))))))))
        (l subexpressions '())))

    ;; case conditional
    (lambda (predicate consequence alternative)
      (cps-convert
       predicate
       (let ((flag (gensym "FLAG")))
         (make-lambda (list flag)
                      (make-conditional flag
                                        (cps-convert consequence continuation)
                                        (cps-convert alternative continuation))))))

    ;; case lambda
    (lambda (bound-variables body)
      (let ((k (gensym "K")))
        (make-application
         `(,@(when *add-funcalls* (list 'funcall))
             ,continuation
             ,(make-lambda `(,@(when (eq *continuation-position* :first)
                                 (list k))
                               ,@bound-variables
                               ,@(when (eq *continuation-position* :last)
                                   (list k)))
                           (cps-convert body k))))))

    ;; case quotation
    (lambda (quoted)
      (make-application `(,@(when *add-funcalls* (list 'funcall)) ,continuation ,expression)))

    ;; case symbol
    (lambda (symbol)
      (make-application `(,@(when *add-funcalls* (list 'funcall)) ,continuation ,expression)))

    ;; case value
    (lambda (expression)
      (make-application `(,@(when *add-funcalls* (list 'funcall)) ,continuation ,expression)))))
