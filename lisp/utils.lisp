;;; Hey Emacs, this is -*- Lisp -*- mode

(in-package "CL-USER")

(defun fold-left-1 (combine initial list)
  (cond ((consp list) (fold-left-1 combine (funcall combine initial (car list)) (cdr list)))
        ((null list) initial)
        (t (error "Dotted tail encountered in fold-left-1: ~s" list))))

(defun fold-left-n (combine initial lists)
  (cond ((every #'consp lists) (fold-left-n combine
                                            (apply combine initial (map 'list #'car lists))
                                            (map 'list #'cdr lists)))
        ((every #'null lists) initial)
        (t (error "Uneven lists encountered in fold-left-n: ~s" lists))))

(defun fold-left (combine initial list &rest lists)
  (if (null lists)
      (fold-left-1 combine initial list)
      (fold-left-n combine initial (cons list lists))))

(defun same-length? (left right)
  (if (consp left)
      (and (consp right)
           (same-length? (cdr left) (cdr right)))
      (not (consp right))))

