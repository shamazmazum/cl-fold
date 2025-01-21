(in-package :cl-fold)

(macrolet ((fold-transform (name)
             (let ((list-fn   (find-symbol (concatenate 'string "LIST-"
                                                        (symbol-name name))))
                   (vector-fn (find-symbol (concatenate 'string "VECTOR-"
                                                        (symbol-name name)))))
               `(sb-c:deftransform ,name ((f init xs) (function t sequence) *)
                 (let ((sequence-type (sb-c::lvar-type xs)))
                   (cond
                     ((sb-kernel:csubtypep sequence-type (sb-kernel:specifier-type 'list))
                      '(,list-fn f init xs))
                     ((sb-kernel:csubtypep sequence-type (sb-kernel:specifier-type 'vector))
                      '(,vector-fn f init xs))
                     (t
                      (sb-c::give-up-ir1-transform))))))))
  (fold-transform foldl)
  (fold-transform foldr))

(macrolet ((fold1-transform (name)
             (let ((list-fn   (find-symbol (concatenate 'string "LIST-"
                                                        (symbol-name name))))
                   (vector-fn (find-symbol (concatenate 'string "VECTOR-"
                                                        (symbol-name name)))))
               `(sb-c:deftransform ,name ((f xs) (function sequence) *)
                 (let ((sequence-type (sb-c::lvar-type xs)))
                   (cond
                     ((sb-kernel:csubtypep sequence-type (sb-kernel:specifier-type 'list))
                      '(,list-fn f xs))
                     ((sb-kernel:csubtypep sequence-type (sb-kernel:specifier-type 'vector))
                      '(,vector-fn f xs))
                     (t
                      (sb-c::give-up-ir1-transform))))))))
  (fold1-transform foldl1)
  (fold1-transform foldr1))

(defun types-intersect-p (&rest types)
  (not (eq (apply #'sb-kernel:type-intersection types) sb-kernel:*empty-type*)))

(defun report-lossage (f-type)
  (let ((sb-c::*lossage-fun* #'sb-c::compiler-warn))
    (sb-c::note-lossage
     "Function type ~a is incompatible with this fold"
     (sb-kernel:type-specifier f-type)))
  sb-kernel:*empty-type*)

(defun type-or-universal (type)
  (if type type sb-kernel:*universal-type*))

;; Very BASIC type derivers
;; Assume that nothing specific is known about the sequence (only that
;; it is a sequence). Otherwise, these folds would be inlined.
(macrolet ((fold1-type-deriver (name place)
             `(sb-c:defoptimizer (,name sb-c:derive-type) ((f xs))
                (let ((f-type (sb-c::lvar-fun-type f t t)))
                  (when (sb-kernel:fun-type-p f-type)
                    (let ((acc-type (type-or-universal
                                     (,place (sb-kernel:fun-type-required f-type))))
                          (ret-type (sb-kernel:single-value-type
                                     (sb-kernel:fun-type-returns f-type))))
                      (if (types-intersect-p acc-type ret-type)
                          ret-type
                          (report-lossage f-type))))))))
  (fold1-type-deriver foldl1 first)
  (fold1-type-deriver foldr1 second))

(macrolet ((fold-type-deriver (name place)
             `(sb-c:defoptimizer (,name sb-c:derive-type) ((f init xs))
                (let ((f-type    (sb-c::lvar-fun-type f t t))
                      (init-type (sb-c::lvar-type init)))
                  (when (sb-kernel:fun-type-p f-type)
                    (let ((acc-type
                           (type-or-universal (,place (sb-kernel:fun-type-required f-type))))
                          (ret-type (sb-kernel:single-value-type
                                     (sb-kernel:fun-type-returns f-type))))
                      ;; Note, that it's tempting to assume that INIT-TYPE must intersect
                      ;; with RET-TYPE. Indeed, in haskell we have
                      ;;
                      ;; foldl :: (a -> b -> a) -> a -> [b] -> a
                      ;;
                      ;; i.e. an equation INIT-TYPE = RET-TYPE.
                      ;; In a type system with subtyping, however, we can have
                      ;; (FOLDL #'CONS NIL '(1 2 3)), so the folding function consumes an
                      ;; accumulator of type T and produces a CONS and the initial value
                      ;; has type NULL. It's clear to see that NULL and CONS do not
                      ;; intersect, despite building together the type LIST.
                      (if (and (types-intersect-p acc-type init-type)
                               (types-intersect-p acc-type ret-type))
                          ret-type
                          (report-lossage f-type))))))))
  (fold-type-deriver foldl first)
  (fold-type-deriver foldr second))
