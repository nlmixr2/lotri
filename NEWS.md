# lotri 1.0.0

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

* New option of `cov` added which check for matrix suitability for
  covariance matrix. When `cov=TRUE`, off-diagonal elements in
  covariance matrices may no longer be nonzero if the diagonal value
  is zero (rxode2#481). This will also check tht the matrix is
  non-positive definite on the non-diagonal terms. `cov` can also be a
  function to allow correction of the matrix to a positive definite
  matrix automatically.

* New option of `rcm`; When enabled, it will permute the matrix to try
  to get a banded matrix using the Reverse Cuthill McKee algorithm.

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
  matrix to a lotri form.

# lotri 0.4.4

* Bug fix for non-standard evaluation where you take the numeric vector from
  the evaluating environment when using `lotri(n1+n2~omega)`

# lotri 0.4.3

* Bug fix for etas that were not named correctly for large order problems

# lotri 0.4.2

* Bug fix for etas that are inconsistently numbered
* Add case for empty initialization block

# lotri 0.4.0

* Can convert lotri objects to data-frames similar to the internal
  data frame used in `nlmixr()`; These can then be converted back with
  `as.lotri()`

* Added the ability to add estimates to a lotri object.  These
  estimates are an attached `data.frame` to the original lotri matrix.
  You can extract them or drop them with the function `lotriEst()`

* Allow specifying fixed components in `lotri()` matrices.

* Add `cov`, `cor`, `sd`, `var`, and `chol` options for matrix
  specification.  The final matrix will always be the covariance matrix

* Add function `lotriMatInv()` which takes a symmetric block matrix
  and converts it into a list of matrices.  An sort of inverse
  operation of `lotriMat()`

* Add error for `lotri(~c(40))`

* Added ability to flag `fixed` and `unfixed` components in a
  matrix. Currently `lotri` only supports one type.

* For the `lotriFix` objects, ie those created with population types
  of estimates and covariance estimates, allow them to be converted to
  an equivalent expression with `as.expression()` for the lotri object
  and `lotriDataFrameToLotriExpression()` for the `data.frame`

# lotri 0.3.1

* Change errors/warnings to use `call.=FALSE` or equivalent.
* Refactor C code to reduce complexity
* Change C code to play nicely with `rchk`
* Allow `lotriMat` to mix named and unnamed matrices; When mixed, an
  unnamed matrix will be returned.

# lotri 0.2.2

* Bug fix for conditional matrices
* Now accessing `$lower` and `$upper` gives default values even if it
  wasn't specified.
* Can change the default conditional matrix to some other value like "id"
* Can add properties to matrix->lotri by as.lotri(matrix, lower=3, default="id")
* Dropped `Matrix` import and added `lotriMat` to create banded
  matrices (faster than `Matrix` for now included repeated matrices
  with `list(matrix, rep)`).
* Bug fix for default properties when both `upper` and `lower` bounds
  are specified

# lotri 0.2.1

* Added conditional matrix specification `|`; Returns a list of matrices
  where each condition is the name of the matrix returned.
* The conditional matrix can also include properties ie `| id(lower=c(eta1=3))`
* Added a `NEWS.md` file to track changes to the package.

# lotri 0.1.1

* Initial Release
