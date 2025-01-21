(in-package :cl-fold)

(sb-c:defknown (foldl foldr) ((function (t t) *) t sequence) t
    (sb-c:foldable sb-c:flushable sb-c:call))

(sb-c:defknown (foldl1 foldr1) ((function (t t) *) sequence) t
    (sb-c:foldable sb-c:call))
