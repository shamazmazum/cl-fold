(defsystem :cl-fold
    :name :cl-fold
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Fast foldl/foldr/foldl1/foldr1 for Common Lisp"
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "fndb" :if-feature :sbcl)
                 (:file "fold")
                 (:file "sbcl" :if-feature :sbcl))
    :in-order-to ((test-op (load-op "cl-fold/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (funcall
                       (symbol-function
                        (intern (symbol-name '#:run-tests)
                                (find-package :cl-fold/tests))))))

(defsystem :cl-fold/tests
    :name :cl-fold/tests
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :serial t
    :pathname "tests"
    :components ((:file "package")
                 (:file "tests"))
    :depends-on (:cl-fold :fiveam))

(defsystem :cl-fold/benchmarks
    :name :cl-fold/benchmarks
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :serial t
    :pathname "benchmarks"
    :components ((:file "package")
                 (:file "benchmarks"))
    :depends-on (:cl-fold :trivial-benchmark))
