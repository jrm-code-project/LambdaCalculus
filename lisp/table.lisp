;;; Hey Emacs, this is -*- Lisp -*- mode

(in-package "CL-USER")

(defun table/empty ()
  (lambda (key if-found if-not-found)
    (declare (ignore key if-found))
    (funcall if-not-found)))

(defun table/extend (table key value)
  (lambda (key* if-found if-not-found)
    (if (or (eql key* key)
            (equal (symbol-name key*) (symbol-name key)))
        (funcall if-found value)
        (funcall table key* if-found if-not-found))))

(defun table/extend* (table keys values)
  (fold-left #'table/extend table keys values))

(defun table/redact (table key)
  (lambda (key* if-found if-not-found)
    (if (eql key* key)
        (funcall if-not-found)
        (funcall table key* if-found if-not-found))))

(defun table/redact* (table keys)
  (fold-left #'table/redact table keys))

(defun table/append (primary secondary)
  (lambda (key if-found if-not-found)
    (funcall primary key
             if-found
             (lambda ()
               (funcall secondary key if-found if-not-found)))))

