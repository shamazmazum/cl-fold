(in-package :cl-fold/tests)

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(fold))))

(def-suite fold :description "Test fold functions")

(defmacro with-setup (&body body)
  `(flet ((f (x y)
            (+ (* 2 x) y)))
     (let* ((list (loop repeat 100 collect (random 10)))
            (vector (coerce list 'vector)))
       ,@body)))

(defmacro with-tests (((var) forms) &body body)
  `(progn
     ,@(loop for form in forms collect
             `(let ((,var ,form))
                ,@body))))

(defmacro test-fold (form)
  `(with-tests ((seq) ((the list list)
                       (the vector vector)
                       ;; Specific type is not know at compile time
                       (if (zerop (random 2)) list vector)))
     ,form))

(in-suite fold)
(test foldl
  (with-setup
    (test-fold
     (is (= (cl-fold:foldl #'f 0 seq)
            (reduce #'f seq :initial-value 0))))
    (is-true (zerop (cl-fold:foldl #'f 0 '())))
    (is-true (zerop (cl-fold:foldl #'f 0 #())))))

(test foldl1
  (with-setup
    (test-fold
     (is (= (cl-fold:foldl1 #'f seq)
            (reduce #'f seq))))
    (signals cl-fold:empty-sequence (cl-fold:foldl1 #'f '()))
    (signals cl-fold:empty-sequence (cl-fold:foldl1 #'f #()))))

(test foldr
  (with-setup
    (test-fold
     (is (= (cl-fold:foldr #'f 0 seq)
           (reduce #'f seq :initial-value 0 :from-end t))))
    (is-true (zerop (cl-fold:foldr #'f 0 '())))
    (is-true (zerop (cl-fold:foldr #'f 0 #())))))

(test foldr1
  (with-setup
    (test-fold
     (is (= (cl-fold:foldr1 #'f seq)
            (reduce #'f seq :from-end t))))
    (signals cl-fold:empty-sequence (cl-fold:foldr1 #'f '()))
    (signals cl-fold:empty-sequence (cl-fold:foldr1 #'f #()))))
