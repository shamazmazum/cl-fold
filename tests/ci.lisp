(defun do-all()
  (ql:quickload :cl-fold/tests)
  (uiop:quit
   (if (uiop:call-function "cl-fold/tests:run-tests")
       0 1)))

(do-all)
