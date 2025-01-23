(in-package :cl-fold)

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
