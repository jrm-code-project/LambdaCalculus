;;; -*- Lisp -*-

(in-package "CL-USER")

(defun make-lambda (bound-variables body)
  `(lambda ,bound-variables ,body))

(defun lambda? (expression)
  (and (consp expression)
       (eql (car expression) 'lambda)))

(defun lambda-bound-variables (expression) (cadr expression))
(defun lambda-body (expression) (caddr expression))

(defun make-application (operator operands)
  (if (and (lambda? operator)
           (same-length? (lambda-bound-variables operator) operands))
      `(LET ,(map 'list (lambda (var val)
                          `(,var ,val))
                  (lambda-bound-variables operator)
                  operands)
         ,(lambda-body operator))
      `(,operator ,@operands)))

(defun application? (expression)
  (and (consp expression)
       (not (eql (car expression) 'if))
       (not (eql (car expression) 'lambda))
       (not (eql (car expression) 'quote))))

(defun application-operator (expression)
  (if (eq (car expression) 'LET)
      (make-lambda (map 'list #'car (cadr expression)) (caddr expression))
      (car expression)))

(defun application-operands (expression)
  (if (eq (car expression) 'let)
      (map 'list #'cadr (cadr expression))
      (cdr expression)))

(defun make-conditional (predicate consequence alternative)
  `(if ,predicate ,consequence ,alternative))

(defun conditional? (expression)
  (and (consp expression)
       (eql (car expression) 'if)))

(defun conditional-predicate (expression) (cadr expression))
(defun conditional-consequence (expression) (caddr expression))
(defun conditional-alternative (expression) (cadddr expression))

(defun make-quotation (data)
  `(quote ,data))

(defun quotation? (expression)
  (and (consp expression)
       (eql (car expression) 'quote)))

(defun quotation-data (expression) (cadr expression))

(defun value? (expression)
  (and (not (symbolp expression))
       (not (consp expression))))

(defun expression-dispatch (expression if-application if-conditional if-lambda if-quotation if-symbol if-value)
  (cond ((symbolp expression)
         (funcall if-symbol expression))
        ((consp expression)
         (cond ((eq (car expression) 'if)
                (funcall if-conditional
                         (conditional-predicate expression)
                         (conditional-consequence expression)
                         (conditional-alternative expression)))
               ((eq (car expression) 'lambda)
                (funcall if-lambda
                         (lambda-bound-variables expression)
                         (lambda-body expression)))
               ((eq (car expression) 'quote)
                (funcall if-quotation
                         (quotation-data expression)))
               (t (funcall if-application
                           (application-operator expression)
                           (application-operands expression)))))
        (t (funcall if-value expression))))
