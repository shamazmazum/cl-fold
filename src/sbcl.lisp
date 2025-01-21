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

;; Very BASIC type deriver
(macrolet ((derive-type (name has-init-p)
             `(sb-c:defoptimizer (,name sb-c:derive-type) ((f ,@(if has-init-p '(init)) xs))
                (let ((f-type (sb-c::lvar-fun-type f t t)))
                  (when (sb-kernel:fun-type-p f-type)
                    (sb-kernel:single-value-type (sb-kernel:fun-type-returns f-type)))))))
  (derive-type foldl1 nil)
  (derive-type foldr1 nil)
  (derive-type foldl t)
  (derive-type foldr t))
