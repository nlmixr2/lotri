# lotri 0.3.1
* Change errors/warnings to use `call.=FALSE` or equivalent. 
* Refactor C code to reduce complexity
* Change C code to play nicely with `rchk`
# lotri 0.2.2
* Bug fix for conditional matrices
* Now accessing `$lower` and `$upper` gives default values even if it
  wasn't specified.
* Can change the default conditional matrix to some other value like "id"
* Can add properties to matrx->lotri by as.lotri(matrix, lower=3, default="id")
* Dropped `Matrix` import and added `lotriMat` to create banded
  matrices (faster than `Matrix` for now included repeated matrices
  with `list(matrix, rep)`).
* Bug fix for default properties when both `upper` and `lower` bounds are specified

# lotri 0.2.1
* Added conditional matrix specification `|`; Returns a list of matrices
  where each condition is the name of the matrix returned.
* The conditional matrix can also include properties ie `| id(lower=c(eta1=3))`
* Added a `NEWS.md` file to track changes to the package.

# lotri 0.1.1
* Initial Release

