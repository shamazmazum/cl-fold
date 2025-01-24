# cl-fold

## About

This library provides basic functions for folding sequences, namely `foldl`,
`foldr`, `foldl1` and `foldr1`. These functions serve as a replacement for the
standard function `reduce`. Also there is `mapfoldl` and `mapfoldr` as a
replacement for reduce with `:key` argument.

Here is a correspondence between various `fold` functions and reduce:

| Fold   |  Reduce   |
|--------|-----------|
| `(foldl f x xs)` | `(reduce f xs :initial-value x)` |
| `(foldr f x xs)` | `(reduce f xs :initial-value x :from-end t)` |
| `(foldl1 f xs)` | `(reduce f xs)` |
| `(foldr1 f xs)` | `(reduce f xs :from-end t)` |
| `(mapfoldl f g x xs)` | `(reduce f xs :initial-value x :key g)` |
| `(mapfoldr f g x xs)` | `(reduce f xs :initial-value x :key g :from-end t)` |

Unlike `reduce`, `fold` functions only accept functions as their first argument
(i.e. they do not accept function designators which are not functions, such as
symbols). Also, these functions must accept exactly two arguments, unlike
functions which are feeded to `reduce` which may be called with zero arguments.

If a sequence `xs` is empty, `foldl1` and `foldr1` signal `empty-sequence`
error. Otherwise, these functions are identical to uses of reduce shown in the
table above.

## SBCL notes

On SBCL these functions are inlined if a sequence type is known at compile
time. Also there is a limited ability to derive types of the result, so
compiling this

``` lisp
(lambda (xs) (length (cl-fold:foldl #'+ 0 xs)))
```

gives a compile-time warning.

### SBCL cannot into inlining

Use `mapfoldl` and `mapfoldr` with care. SBCL often fails to inline their second
argument. For more info, see
[here](https://bugs.launchpad.net/sbcl/+bug/2095560). For example, instead of

``` lisp
(cl-fold:mapfoldl #'f #'g x xs)
```

you will probably need to write

``` lisp
(cl-fold:mapfoldl #'f (lambda (x) (g x)) x xs)
```

## Benchmarks

These benchmarks are for summing all elements in a list and a simple vector with
`foldl`, `foldr` and `foldr1`. One measurement consists of running `foldXX`
400000 times with a test sequence with 200 elements. Results are averaged over
100 measurements. Source code for the benchmarks is in `cl-fold/benchmarks`
package.

Running this code on my system gives [these results](benchmarks.md)
