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

(in-suite fold)
(test foldl
  (with-setup
    (is (= (cl-fold:foldl #'f 0 list)
           (reduce #'f list :initial-value 0)))
    (is (= (cl-fold:foldl #'f 0 vector)
           (reduce #'f vector :initial-value 0)))
    (is-true (zerop (cl-fold:foldl #'f 0 '())))
    (is-true (zerop (cl-fold:foldl #'f 0 #())))))

(test foldl1
  (with-setup
    (is (= (cl-fold:foldl1 #'f list)
           (reduce #'f list)))
    (is (= (cl-fold:foldl1 #'f vector)
           (reduce #'f vector)))
    (signals cl-fold:empty-sequence (cl-fold:foldl1 #'f '()))
    (signals cl-fold:empty-sequence (cl-fold:foldl1 #'f #()))))

(test foldr
  (with-setup
    (is (= (cl-fold:foldr #'f 0 list)
           (reduce #'f list :initial-value 0 :from-end t)))
    (is (= (cl-fold:foldr #'f 0 vector)
           (reduce #'f vector :initial-value 0 :from-end t)))
    (is-true (zerop (cl-fold:foldr #'f 0 '())))
    (is-true (zerop (cl-fold:foldr #'f 0 #())))))

(test foldr1
  (with-setup
    (is (= (cl-fold:foldr1 #'f list)
           (reduce #'f list :from-end t)))
    (is (= (cl-fold:foldr1 #'f vector)
           (reduce #'f vector :from-end t)))
    (signals cl-fold:empty-sequence (cl-fold:foldr1 #'f '()))
    (signals cl-fold:empty-sequence (cl-fold:foldr1 #'f #()))))
