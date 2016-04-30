repl for displaying tree-like derivations of lambda terms with type and/or
normal-form annotations.

nearly all of the type inference and repl code is from [write you a
haskell](https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter7/poly_constraints).

the cofree technique for annotations is from [this blog
post](https://brianmckenna.org/blog/type_annotation_cofree).

and the de bruijn encoding and normalizing routine come basically straight from
the examples in the [bound](http://hackage.haskell.org/package/bound) library.
