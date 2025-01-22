(in-package :cl-fold)

;; Error condition for partial functions
(define-condition empty-sequence (error)
  ((fn :reader  empty-sequence-fn
       :initarg :fn
       :type    symbol))
  (:report (lambda (c s)
             (format s "~a: The sequence is empty"
                     (empty-sequence-fn c)))))

;; List variant
(declaim (inline list-foldl))
(defun list-foldl (f init xs)
  (labels ((%go (x xs)
             (if (null xs) x
                 (%go (funcall f x (car xs)) (cdr xs)))))
    (%go init xs)))

(declaim (inline list-foldl1))
(defun list-foldl1 (f xs)
  (when (null xs)
    (error 'empty-sequence :fn 'foldl1))
  (list-foldl f (car xs) (cdr xs)))

(declaim (inline list-foldr))
(defun list-foldr (f init xs)
  (labels ((%go (xs)
             (if (null xs) init
                 (funcall f (car xs) (%go (cdr xs))))))
    (%go xs)))

(declaim (inline list-foldr1))
(defun list-foldr1 (f xs)
  (when (null xs)
    (error 'empty-sequence :fn 'foldr1))
  ;; Transform to FOLDL to do the fold and find initial element in one pass.
  (labels ((%go (g xs)
             (if (null (cdr xs))
                 (values g (car xs))
                 (%go (lambda (y) (funcall g (funcall f (car xs) y))) (cdr xs)))))
    (multiple-value-bind (g init)
        (%go #'identity xs)
      (funcall g init))))

;; Vector variants
(declaim (inline vector-foldl))
(defun vector-foldl (f init v &key (start 0))
  (let ((result init))
    (do ((i start (1+ i)))
        ((= i (length v)) result)
      (setq result (funcall f result (elt v i))))))

(declaim (inline vector-foldl1))
(defun vector-foldl1 (f v)
  (when (zerop (length v))
    (error 'empty-sequence :fn 'foldl1))
  (vector-foldl f (elt v 0) v :start 1))

(declaim (inline vector-foldr))
(defun vector-foldr (f init v &key (end (length v)))
  (let ((result init))
    (do ((i 0 (1+ i)))
        ((= i end) result)
      (setq result (funcall f (elt v (- end i 1)) result)))))

(declaim (inline vector-foldr1))
(defun vector-foldr1 (f v)
  (let ((length (length v)))
    (when (zerop length)
      (error 'empty-sequence :fn 'foldr1))
    (vector-foldr f (elt v (1- length)) v :end (1- length))))

;; Generic functions
(defun foldl (f init xs)
  "Left fold.

For a sequence XS = [X0, X1, ...] compute F(F(F(INIT, X0), X1), ...)."
  (declare (type function f))
  (typecase xs
    (list   (list-foldl   f init xs))
    (vector (vector-foldl f init xs))))

(defun foldr (f init xs)
  "Right fold.

For a sequence XS = [X0, ..., XN-1, XN] compute F(X0, ...(F(XN-1, F(XN, INIT))))."
  (declare (type function f))
  (typecase xs
    (list   (list-foldr   f init xs))
    (vector (vector-foldr f init xs))))

(defun foldl1 (f xs)
  "FOLDL1(F, XS) = FOLDL(F, HEAD(XS), TAIL(XS)).

Signals EMPTY-SEQUENCE if XS is empty."
  (declare (type function f))
  (typecase xs
    (list   (list-foldl1   f xs))
    (vector (vector-foldl1 f xs))))

(defun foldr1 (f xs)
  "FOLDR1(F, XS) = FOLDR(F, HEAD(XS), TAIL(XS)).

Signals EMPTY-SEQUENCE if XS is empty."
  (declare (type function f))
  (typecase xs
    (list   (list-foldr1   f xs))
    (vector (vector-foldr1 f xs))))
