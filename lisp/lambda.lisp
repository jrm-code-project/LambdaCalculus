;;; Hey Emacs, this is -*- Lisp -*- mode

(in-package "CL-USER")

(defun closure? (thing)
  (or (lambda? thing)
      (and (application? thing)
           (lambda? (application-operator thing))
           (same-length? (lambda-bound-variables (application-operator thing))
                         (application-operands thing))
           (closure? (lambda-body (application-operator thing))))))

(defun beta (expression if-reduced if-not-reduced)
  (if (application? expression)
      (let ((operator (application-operator expression))
            (operands (application-operands expression)))
        (if (lambda? operator)
            (let ((bound-variables (lambda-bound-variables operator))
                  (body (lambda-body operator)))
              (if (same-length? bound-variables operands)
                  (funcall if-reduced
                           (xsubst body
                                   (table/extend* (table/empty)
                                                  bound-variables
                                                  operands)))
                  (funcall if-not-reduced)))
            (funcall if-not-reduced)))
      (funcall if-not-reduced)))

(defun beta-step (expression if-reduced if-not-reduced)
  (if (application? expression)
      (let ((operator (application-operator expression))
            (operands (application-operands expression)))
        (if (lambda? operator)
            (let ((bound-variables (lambda-bound-variables operator))
                  (body (lambda-body operator)))
              (if (same-length? bound-variables operands)
                  (funcall if-reduced
                           (xsubst-step body
                                   (table/extend* (table/empty)
                                                  bound-variables
                                                  operands)))
                  (funcall if-not-reduced)))
            (funcall if-not-reduced)))
      (funcall if-not-reduced)))

(defun partial-beta (predicate)
  (lambda (expression if-reduced if-not-reduced)
    (if (application? expression)
        (let ((operator (application-operator expression))
              (operands (application-operands expression)))
          (if (lambda? operator)
              (let ((bound-variables (lambda-bound-variables operator))
                    (body (lambda-body operator)))
                (if (same-length? bound-variables operands)
                    (labels ((l (bound-variables operands table new-bound-variables new-operands)
                               (if (null bound-variables)
                                   (if (null new-bound-variables)
                                       (funcall if-reduced (xsubst body table))
                                       (funcall if-reduced
                                                (make-application
                                                 (xsubst (make-lambda (reverse new-bound-variables) body) table)
                                                 (reverse new-operands))))
                                   (if (funcall predicate (car bound-variables) (car operands))
                                       (l (cdr bound-variables)
                                          (cdr operands)
                                          (table/extend table (car bound-variables) (car operands))
                                          new-bound-variables
                                          new-operands)
                                       (l (cdr bound-variables)
                                          (cdr operands)
                                          table
                                          (cons (car bound-variables) new-bound-variables)
                                          (cons (car operands) new-operands))))))
                      (l bound-variables operands (table/empty) '() '()))
                    (funcall if-not-reduced)))
              (funcall if-not-reduced)))
        (funcall if-not-reduced))))

(defun test-partial-beta ()
  (funcall
   (partial-beta (lambda (var val) (declare (ignore var)) (symbolp val)))
   '((lambda (a b c) (+ a b c)) x (+ y 7) z)
   (lambda (answer)
     (assert (equal answer '((lambda (b) (+ x b z)) (+ y 7))))
     t)
   (constantly nil)))

(defun eta (expression if-reduced if-not-reduced)
  (if (lambda? expression)
      (let ((bound-variables (cadr expression))
            (body (caddr expression)))
        (if (application? body)
            (let ((operator (car body))
                  (operands (cdr body)))
              (if (and (null (intersection bound-variables (free-variables operator)))
                       (equal bound-variables operands))
                  (funcall if-reduced operator)
                  (funcall if-not-reduced)))
            (funcall if-not-reduced)))
      (funcall if-not-reduced)))

(defun delta-conditional (expression if-reduced if-not-reduced)
  (if (conditional? expression)
      (let ((predicate (cadr expression))
            (consequence (caddr expression))
            (alternative (cadddr expression)))
        (cond ((lambda? predicate) (funcall if-reduced consequence))
              ((quotation? predicate) (if (eql (cadr predicate) 'nil)
                                          (funcall if-reduced alternative)
                                          (funcall if-reduced consequence)))
              ((symbolp predicate) (funcall if-not-reduced))
              ((application? predicate) (funcall if-not-reduced))
              ((eql predicate 'nil) (funcall if-reduced alternative))
              (t (funcall if-reduced consequence))))
      (funcall if-not-reduced)))

(defun delta-value (expression if-reduced if-not-reduced)
  (if (application? expression)
      (let ((operator (car expression))
            (operands (cdr expression)))
        (if (and (or (typep operator 'function)
                     (quotation? operator)
                     (typep (cadr operator) 'function))
                 (every (lambda (operand)
                          (or (lambda? operand)
                              (quotation? operand)
                              (and (not (consp operand))
                                   (not (symbolp operand))))) operands))
            (funcall
             (handler-case
                 (let* ((raw-answer
                         (apply (if (quotation? operator)
                                    (cadr operator)
                                    operator)
                                (map 'list (lambda (operand)
                                             (if (quotation? operand)
                                                 (cadr operand)
                                                 operand))
                                     operands)))
                        (answer (if (or (symbolp raw-answer)
                                        (consp raw-answer))
                                    (make-quotation raw-answer)
                                    raw-answer)))
                   (lambda ()
                     (funcall if-reduced answer)))
               (error () if-not-reduced)))
            (funcall if-not-reduced)))
      (funcall if-not-reduced)))

(defun beta-normalize-step (expression)
  (expression-dispatch expression

    ;; case application
    (lambda (operator operands)
      ;; First, try to beta reduce the application
      (beta-step expression
            #'identity
            ;; If we cannot beta reduce, recursively descend each subexpression
            ;; in turn from left to right until one reduces.
            (lambda ()
              (let ((new-operator (beta-normalize-step operator)))
                (if (eq new-operator operator)
                    (labels ((l (subexpressions)
                               (if (null subexpressions)
                                   '()
                                   (let ((new-sub (beta-normalize-step (car subexpressions))))
                                     (if (eq new-sub (car subexpressions))
                                         (let ((new-tail (l (cdr subexpressions))))
                                           (if (eq new-tail (cdr subexpressions))
                                               subexpressions
                                               (cons (car subexpressions) new-tail)))
                                         (cons new-sub (cdr subexpressions)))))))
                      (let ((new-operands (l operands)))
                        (if (eq new-operands operands)
                            expression
                            (make-application new-operator new-operands))))
                    (make-application new-operator operands))))))


    ;; case conditional
    (lambda (predicate consequence alternative)
      (let ((new-predicate (beta-normalize-step predicate)))
        (if (eq new-predicate predicate)
            (let ((new-consequence (beta-normalize-step consequence)))
              (if (eq new-consequence consequence)
                  (let ((new-alternative (beta-normalize-step alternative)))
                    (if (eq new-alternative alternative)
                        expression
                        (make-conditional predicate consequence new-alternative)))
                  (make-conditional predicate new-consequence alternative)))
            (make-conditional new-predicate consequence alternative))))

    ;; case lambda
    (lambda (bound-variables body)
      (let ((new-body (beta-normalize-step body)))
        (if (eql new-body body)
            expression
            (make-lambda bound-variables new-body))))

    ;; case quotation
    (constantly expression)

    ;; case symbol
    (constantly expression)

    ;; case value
    (constantly expression)))

(defun beta-normalize (expression)
  (do ((expression expression (beta-normalize-step expression))
       (expression1 '() expression)
       (count 0 (+ count 1)))
      ((eq expression expression1)
       (format t "~%~d beta reductions" (- count 1))
       expression)))

(defun test-form ()
  (let ((table
         (table/extend*
          (table/empty)
          '(*
            five
            one
            pred
            three
            zero?
            y)
          '(

            ;; *
            (lambda (m n)
              (lambda (f)
                (m (n f))))

            ;; Church numeral five
            (lambda (f) (lambda (x) (f (f (f (f (f x)))))))

            ;; Church numeral one
            (lambda (f) (lambda (x) (f x)))

            ;; pred (subtract 1)
            (lambda (n)
              (lambda (f)
                (lambda (x) (((n (lambda (g)
                                   (lambda (h)
                                     (h (g f)))))
                              (lambda (u) x))
                             (lambda (u) u)))))

            ;; Church numeral three 
            (lambda (f) (lambda (x) (f (f (f x)))))

            ;; zero?
            (lambda (n t f) ((n (lambda (x) f)) t))

            ;; Y
            (lambda (f)
              ((lambda (x) (f (x x)))
               (lambda (x) (f (x x)))))
            )))
        (expr
         '((lambda (factorial)
             (factorial three))
           (y (lambda (fact)
                (lambda (x)
                  (zero? x
                   one
                   (* (fact (pred x)) x))))))))
    (xsubst expr table)))

(defun jeval (expression)
  (do ((expr expression (beta-normalize-step expr))
       (count -1 (+ count 1))
       (expr1 '() expr))
      ((eq expr expr1)
       (format t "~%~d reductions." count)
       expr)
    (format t "~%~d~%~s" (+ count 1) expr)
    ))

(defun testit ()
  (jeval (test-form)))

