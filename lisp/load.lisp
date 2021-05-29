(let ((*default-pathname-defaults*
       (make-pathname
        :host (pathname-host *load-pathname*)
        :device (pathname-device *load-pathname*)
        :directory (pathname-directory *load-pathname*))))
  (load "utils")
  (load "table")
  (load "expression")
  (load "xsubst")
  (load "lambda"))
