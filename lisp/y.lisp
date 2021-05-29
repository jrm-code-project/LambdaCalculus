;;; -*- Lisp -*-

(in-package "CL-USER")

;;; Curry's Y combinator
;;;
;;;  y = λg.(λf.g(f f))(λf.g(f f))
;;;

;;; Normal order lisp
;;; (defun y (g)
;;;   ((lambda (f) (funcall g (funcall f f)))
;;;    (lambda (f) (funcall g (funcall f f)))))

;;; Avoid infinite recursion by η-expanding recursive call
(defun y (g)
  ((lambda (f) (lambda (&rest args) (apply (funcall g (funcall f f)) args)))
   (lambda (f) (lambda (&rest args) (apply (funcall g (funcall f f)) args)))))

(defun y-eta (g)
  (let ((d (lambda (f) (lambda (&rest args) (apply (funcall g (funcall f f)) args)))))
    (funcall d d)))

;;; Return a list of mutually recursive functions
(defun y* (&rest gs)
  ((lambda (u) (funcall u u))
   (lambda (f)
     (map 'list (lambda (g)
                  (lambda (&rest args) (apply (apply g (funcall f f)) args)))
          gs))))

;;; Return mutually recursive functions as values
(defun y** (&rest gs)
  ((lambda (u) (funcall u u))
   (lambda (f)
     (apply #'values
            (map 'list
                 (lambda (g)
                   (lambda (&rest args)
                     (apply (multiple-value-call g (funcall f f)) args)))
                 gs)))))

(defmacro named-lambda ((name &rest args) &body body)
  `(y-eta (lambda (,name)
            (lambda ,args
              ,@body))))

(defmacro named-let (name bvl &body body)
  `(funcall (named-lambda (,name ,@(map #'car bvl)) ,@body)
            ,@(map 'list #'(lambda (binding)
                             (cons 'progn (cdr binding)))
                   bvl)))


(let ((even-odd
       (y* (lambda (even? odd?)
             (declare (ignore even?))
             (lambda (x)
               (or (zerop x)
                   (funcall odd? (- x 1)))))

           (lambda (even? odd?)
             (declare (ignore odd?))
             (lambda (x)
               (and (not (zerop x))
                    (funcall even? (- x 1))))))))
  (let ((even? (car even-odd))
        (odd? (cadr even-odd)))
    (do ((i 10 (+ i 1)))
        ((>= i 15))
      (format t "~%~d, ~s ~s" i (funcall even? i) (funcall odd? i)))))


(funcall
  (y (lambda (factorial)
         (lambda (x)
           (if (zerop x)
               1
               (* x (funcall factorial (- x 1)))))))
 6)

(funcall
 (y (lambda (delayed-factorial)
      (flet ((factorial (&rest args)
               (apply (funcall delayed-factorial) args)))

        (lambda (x)
          (if (zerop x)
              1
              (* x (factorial (- x 1))))))))
 6)

;;; Applicative order by eta expansion
(defun y1 (g)
  ((lambda (f) (funcall g (lambda (&rest args) (apply (funcall f f) args))))
   (lambda (f) (funcall g (lambda (&rest args) (apply (funcall f f) args))))))

(funcall
 (y1 (lambda (factorial)
       (lambda (x)
         (if (zerop x)
             1
             (* x (funcall factorial (- x 1)))))))
 6)

;;; Mutual recursion by return of multiple values
(multiple-value-bind (even? odd?)
    (y (lambda (delayed-recursion)
         (values

          ;; even?
          (lambda (x)
            (or (zerop x)
                (multiple-value-bind (even? odd?) (funcall delayed-recursion)
                  (declare (ignore even?))
                  (funcall odd? (- x 1)))))

          ;; odd?
          (lambda (x)
            (and (not (zerop x))
                 (multiple-value-bind (even? odd?) (funcall delayed-recursion)
                   (declare (ignore odd?))
                   (funcall even? (- x 1)))))
          )))
  (do ((i 0 (+ i 1)))
      ((>= i 5))
    (format t "~%~d = ~s ~s" i (funcall even? i) (funcall odd? i))))

(multiple-value-bind (even? odd?)
    (y (lambda (delayed-recursion)
         (flet ((even? (lambda (x)
                         (multiple-value-bind (even? odd?) (delayed-recursion)
                           (funcall even? x))))
                (odd? (lambda (x)
                        (multiple-value-bind (even? odd?) (delayed-recursion)
                          (funcall odd? x)))))
           (values

            ;; even?
            (lambda (x)
              (or (zerop x)
                  (odd? (- x 1))))

            ;; odd?
            (lambda (x)
              (and (not (zerop x))
                   (even? (- x 1))))
            ))))
  (do ((i 0 (+ i 1)))
      ((>= i 5))
    (format t "~%~d = ~s ~s" i (funcall even? i) (funcall odd? i))))

(defun y2 (g)
  ((lambda (f) (multiple-value-call g (funcall f f)))
   (lambda (f) (multiple-value-call g (funcall f f)))))

(multiple-value-bind (even? odd?)
    (y2 (lambda (delayed-even delayed-odd)
         (values

          ;; even?
          (lambda (x)
            (or (zerop x)
                (funcall (funcall delayed-odd) (- x 1))))

          ;; odd?
          (lambda (x)
            (and (not (zerop x))
                 (funcall (funcall delayed-even) (- x 1))))
          )))
  (do ((i 0 (+ i 1)))
      ((>= i 5))
    (format t "~%~d = ~s ~s" i (funcall even? i) (funcall odd? i))))

(defun cps-y1 (g k)
  (


(defun cps-y (g k)
  ((lambda (f k) (funcall g (lambda (k) (funcall f f k)) k))
   (lambda (f k) (funcall g (lambda (k) (funcall f f k)) k))
   k))

(cps-y (lambda (recur k)
         (funcall
          k
          (lambda (x fk)
            (if (zerop x)
                (funcall fk 1)
                (funcall recur (lambda (fact)
                                (funcall fact (- x 1)
                                         (lambda (res)
                                           (funcall fk (* x res))))))))))
       (lambda (fact)
         (funcall fact 6 #'identity)))



                   
             


(defun delta-cps (f receiver)
  (funcall f f receiver))

(defun y (g)
  "Applicative order Y operator."
  ((lambda (f) (funcall g (lambda () (funcall f f))))
   (lambda (f) (funcall g (lambda () (funcall f f))))))

(defun Y (f)
  ((lambda (x) (lambda (n) (funcall (funcall f (funcall x x)) n)))
   (lambda (x) (lambda (n) (funcall (funcall f (funcall x x)) n)))))

(defun y1 (g)
  ((lambda (f) (funcall g (lambda (&rest args) (apply (funcall f f) args))))
   (lambda (f) (funcall g (lambda (&rest args) (apply (funcall f f) args))))))
  



(defun test1 ()
  (let ((fact (y (lambda (factorial)
                   (lambda (x)
                     (if (zerop x)
                         1
                         (* x (funcall (funcall factorial) (- x 1)))))))))
    (funcall fact 4)))

(defun testit ()
  (let ((eo (funcall (y (lambda (even/odd)
                          (lambda (receiver)
                            (funcall
                             receiver
                             (lambda (x)
                               (or (zerop x)
                                   (funcall (funcall even/odd)
                                            (lambda (even? odd?)
                                              (declare (ignore even?))
                                              (funcall odd? (- x 1))))))
                             (lambda (x)
                               (and (not (zerop x))
                                    (funcall (funcall even/odd)
                                             (lambda (even? odd?)
                                               (declare (ignore odd?))
                                               (funcall even? (- x 1))))))))))
                     (lambda (even? odd?)
                       (declare (ignore odd?))
                       even?))))
    (do ((i 0 (+ i 1)))
        ((>= i 10))
      (format t "~%eo ~d = ~s" i (funcall eo i)))))
