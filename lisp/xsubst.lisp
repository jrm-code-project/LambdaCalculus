;;; -*- Lisp -*-

(in-package "CL-USER")

(defun free-variables (expression)
  (expression-dispatch expression

    ;; case application
    (lambda (operator operands)
      (union
       (free-variables operator)
       (fold-left #'union '() (map 'list #'free-variables operands))))

    ;; case conditional
    (lambda (predicate consequence alternative)
      (union
       (free-variables predicate)
       (union
        (free-variables consequence)
        (free-variables alternative))))

    ;; case lambda
    (lambda (bound-variables body)
      (set-difference (free-variables body) bound-variables))

    ;; case quotation
    (constantly nil)

    ;; case symbol
    #'list

    ;; case value
    (constantly nil)))

(defun make-alpha-table (variables)
  (fold-left
   (lambda (table variable)
     (table/extend table variable (gensym (symbol-name variable))))
   (table/empty)
   variables))

(defun xsubst (expression table)
  (expression-dispatch expression
    ;; case application
    (lambda (operator operands)
      (make-application
       (xsubst operator table)
       (map 'list (lambda (subexpression)
                    (xsubst subexpression table))
            operands)))
    
    ;; case conditional
    (lambda (predicate consequence alternative)
      (make-conditional (xsubst predicate table)
                        (xsubst consequence table)
                        (xsubst alternative table)))

    ;; case lambda
    (lambda (bound-variables body)
      (let ((alpha-table
             (make-alpha-table
              (intersection
               bound-variables
               (fold-left #'union '()
                          (map 'list (lambda (var)
                                       (funcall table var
                                                #'free-variables
                                                (constantly '())))
                               (set-difference (free-variables body) bound-variables)))))))
        (make-lambda
         (map 'list (lambda (symbol)
                      (funcall alpha-table symbol #'identity (constantly symbol)))
              bound-variables)
         (xsubst body
                 (table/append alpha-table (table/redact* table bound-variables))))))

    ;; case quotation
    (constantly expression)

    ;; case symbol
    (lambda (symbol)
      (funcall table symbol #'identity (constantly expression)))

    ;; case value
    (constantly expression)))

(defvar *delay-xsubst-avoid-identities* 't)

(defun delay-xsubst (expression table)
  (if (and (symbolp expression)
           *delay-xsubst-avoid-identities*)
      (funcall table expression #'identity (constantly expression))
      (labels ((l (vars bvl args)
                 (cond ((consp vars)
                        (funcall table
                                 (car vars)
                                 (lambda (expr)
                                   (l (cdr vars) (cons (car vars) bvl) (cons expr args)))
                                 (lambda ()
                                   (l (cdr vars) bvl args))))
                       ((null vars)
                        (if (null bvl)
                            expression
                            (make-application (make-lambda bvl expression)
                                              args)))
                       (t (error "Bad argument list.")))))
        (l (free-variables expression) '() '()))))

(defun xsubst-step (expression table)
  (expression-dispatch expression

    ;; case application
    (lambda (operator operands)
      (let ((delayed-operands (map 'list (lambda (arg)
                                           (delay-xsubst arg table))
                                   operands)))
        (if (lambda? operator)
            (let ((bvl (lambda-bound-variables operator))
                  (body (lambda-body operator)))
              (labels ((l (free-vars new-bound new-args)
                         (if (null free-vars)
                             (if (null new-bound)
                                 (make-application operator delayed-operands)
                                 (make-application
                                  (make-lambda (append new-bound bvl) body)
                                  (append new-args delayed-operands)))
                             (funcall table (car free-vars)
                                      (lambda (val)
                                        (l (cdr free-vars)
                                           (cons (car free-vars) new-bound)
                                           (cons val new-args)))
                                      (lambda ()
                                        (l (cdr free-vars)
                                           new-bound
                                           new-args))))))
                (l (free-variables operator) '() '())))
            (make-application (delay-xsubst operator table) delayed-operands))))

    ;; case conditional
    (lambda (predicate consequence alternative)
      (make-conditional (delay-xsubst predicate table)
                        (delay-xsubst consequence table)
                        (delay-xsubst alternative table)))

    ;; case lambda
    (lambda (bound-variables body)
      (let ((alpha-table
             (make-alpha-table
              (intersection
               bound-variables
               (fold-left #'union '()
                          (map 'list (lambda (var)
                                       (funcall table var
                                                #'free-variables
                                                (constantly '())))
                               (set-difference (free-variables body) bound-variables)))))))
        (make-lambda
         (map 'list (lambda (symbol)
                      (funcall alpha-table symbol #'identity (constantly symbol)))
              bound-variables)
         (delay-xsubst body
                       (table/append alpha-table (table/redact* table bound-variables))))))

    ;; case quotation
    (constantly expression)

    ;; case symbol
    (lambda (symbol)
      (funcall table symbol #'identity (constantly expression)))

    ;; case value
    (constantly expression)))
