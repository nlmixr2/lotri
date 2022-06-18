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

