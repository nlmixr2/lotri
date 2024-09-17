
* This version will allow 'rxode2' to link to 'lori' without abi
  linkage (will help CRAN and my maintainence)

* Added a new way of specifying lotri matrices:

Before you could specify matrices as:

```r
m <- lotri({
  a + b ~ c(1,
            0.5, 1)
})
```

Now you can specify per row as:
```r
m <- lotri({
  a ~ 1
  b ~ c(0.5, 1)
})
```

This form is now the default when converting from a matrix to a lotri
expression. In addition if the matrix is large enough (by default a
5x5 matrix), these would be named when changing them to an expression:

```r
m <- lotri({
  a ~ c(a=1)
  b ~ c(a=0.5, b=1)
  c ~ c(a=0.5, b=0.5, c=1)
  d ~ c(a=0.5, b=0.5, c=0.5, d=1)
  e ~ c(a=0.5, b=0.5, c=0.5, d=1,
        e=1)
})
```

This way changing to an R parsed expression will be rendered in a more
human readable format.

You can change the deparsing options that are used by default with
`lotri` with `options(lotri.plusNames=TRUE)` which prefers the `a+b+c`
syntax when deparsing. Otherwise, the line format is used by default.
The dimension number before naming the values in the line-format can
be controlled with `options(lotri.nameEst=2)` or some other dimension.

* New option of `cov` added which check for matrix suitability for
  covariance matrix. When `cov=TRUE`, off-diagonal elements in
  covariance matrices may no longer be nonzero if the diagonal value
  is zero (rxode2#481). This will also check tht the matrix is
  non-positive definite on the non-diagonal terms. `cov` can also be a
  function to allow correction of the matrix to a positive definite
  matrix automatically.

* New option of `rcm`; When enabled, and `lotriIsBlockMat()` is not
  true, `lotri()` will permute the matrix to try to get a banded
  matrix using the Reverse Cuthill McKee algorithm.

* Change internals for `lotri` so that new `rxode2` is no longer
  required to be binary linked to `lotri`.

* Add new function `rcm()` which permutes the matrix to get a band
  matrix (if possible).  This uses the Reverse Reverse Cuthill McKee
  (RCM) algorithm.

* Moved `nmNearPD()` to this package and renamed to `lotriNearPD()`.
  In addition to moving, this function will now retain the dimension
  names.

* New exported function `lotriAsExpression()` which has more fine
  control than `as.expression()` and will work without converting the
  matrix to a lotri form.  This by default uses the new line form, but
  can be changed back to the `option(lotri.plusNames=TRUE)`.  Also be
  default it will name each element in a matrix when the dimension is
  above `5x5`.  You can change that number by
  `option(lotri.nameEst=10)` to increase it to be named above
  `10x10`. If you do not like the naming you can also disable it with
  `option(lotri.nameEst=TRUE)`, or if you always want it on you can use `option(lotri.nameEst=FALSE)


* This is a bug-fix release for nlmixr2 piping
