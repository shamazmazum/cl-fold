(defpackage cl-fold/benchmarks
  (:use #:cl)
  (:local-nicknames (#:tb #:org.shirakumo.trivial-benchmark))
  (:export #:foldl #:foldr #:foldr1 #:mapfoldl #:mapfoldr))
