# lotri 1.0.5

## New features

* A block can now be repeated with `same()`, which is NONMEM's
  `$OMEGA BLOCK(n) SAME`:

```r
lotri({
  iov.cl1 + iov.v1 ~ c(0.1,
                       0.01, 0.2)
  iov.cl2 + iov.v2 ~ same()
  iov.cl3 + iov.v3 ~ same()
})
```

  This is one estimated 2x2 covariance shared by three blocks, which is
  how inter-occasion variability is parameterized when every occasion
  draws its own random effects from one shared covariance -- and it is
  what lets those random effects be *correlated*.

  `same()` repeats the immediately preceding block under new names and
  takes no arguments.  A further `same()` repeats that same original
  block rather than the copy, the way NONMEM chains `SAME`.  It works
  with a condition (`iov.cl2 + iov.v2 ~ same() | occ`), it inherits the
  fixed flags of the block it repeats, and it composes with the
  `cnd(same = n)` nesting property that `lotriSep()` uses.

* **The `condition` column can now carry a `:same:` suffix.**  Rather
  than adding a column to the data frame from `as.data.frame()`, a
  repeated block records the element it mirrors in the existing
  `condition` column, naming it:

  ```
  <baseCondition>:same:<masterEta>                 # diagonal row
  <baseCondition>:same:<masterEta1>:<masterEta2>   # covariance row
  ```

  The master is named rather than indexed because `neta1`/`neta2` are
  renumbered whenever parameters are added, dropped or reordered.  A
  copy keeps its master's `est` and `fix`, so a consumer that does not
  understand the suffix still sees a numerically correct matrix -- just
  with more free parameters than the model really has.

  Four helpers are exported for reading that column, and code that
  compares `condition` directly (`condition == "id"`) should move to
  them, since such a test misclassifies a repeated block:

  - `lotriBaseCondition()` strips the suffix,
  - `lotriIsSame()` says whether a row mirrors another,
  - `lotriSameMap()` maps each eta to the eta it mirrors,
  - `lotriSameBreak()` drops the linkage for a block that has been
    structurally changed.

  `same()` round trips through `as.data.frame()`, `as.lotri()` and
  `as.expression()`, and the linkage is carried across `lotriMat()`.
  A repetition that no longer describes a real mirror -- because the
  blocks it was built from were reordered or dropped, or because it
  carries something `same()` cannot express -- is written out with its
  explicit values instead.  That loses the annotation, never the
  numbers.

  A prior cannot be put on a repeated block -- it is not a parameter of
  its own, so the prior belongs on the block it mirrors.  Without this
  a model could carry two different priors on what is one estimated
  parameter.

  `same()` cannot be combined with `rcm=TRUE` or with a `cov` function;
  both would move a repeated block away from the block it repeats, and
  are refused rather than silently producing a matrix whose repetition
  is no longer true.


*  Prior distributions can now be specified in a `lotri({})` (and
  therefore `ini({})`) block with `prior(name) ~ dist(...)`, ie:

```r
lotri({
  tka <- 0.45
  tcl <- c(0, 1, 10)
  eta.cl + eta.v ~ c(0.1,
                     0.01, 0.2)
  prior(tka) ~ dnorm(0, 10)
  prior(tcl) ~ dlnorm(1, 0.5)
  prior(eta.cl, eta.v) ~ lkjCorr(2)
})
```

  Because the statement names its target, prior lines may be given
  anywhere in the block.  Priors may be put on population estimates,
  on individual etas, and on whole covariance blocks (with the
  matrix-valued distributions like `lkjCorr()` and `invWishart()`).

  Every distribution has three accepted spellings: the R name where R
  parameterizes it the same way 'Stan' does (`dnorm()`, `dlnorm()`,
  `dgamma()`), the camelCase name (`invWishart()`, `lkjCorr()`,
  `studentT()`), and the 'Stan' name itself (`inv_wishart()`,
  `lkj_corr()`, `student_t()`).  The canonical spelling, which is what
  is stored and printed back, is the R one where there is a faithful
  one and the camelCase one otherwise.  Both positional and named
  arguments work.  `lotri` validates the distribution name, its arity,
  and its support against the parameter's bounds; it does not generate
  any 'Stan' code.

* An inverse Wishart prior on an omega block can be given by its
  degrees of freedom alone, since the block it is put on already is the
  scale matrix.  This is the `$OMEGAP`/`$OMEGAPD` pair of a NONMEM
  `NWPRI` model:

```r
lotri({
  eta.cl + eta.v ~ c(0.1,
                     0.01, 0.2)
  prior(eta.cl, eta.v) ~ invWishart(4)
})
```

  It works on a 1x1 block too (an inverse Wishart of dimension one is
  an inverse gamma), and an improper `nu <= p - 1` is an error.

* A normal prior can be put on the omega values themselves, which is
  what a NONMEM `TNPRI` model needs.  `om.` prepended to a between
  subject variability names its omega element, so the normal prior
  shorthand can be used on it:

```r
lotri({
  eta.cl ~ 0.3
  eta.v ~ 0.1
  om.eta.cl ~ 0.01
  om.eta.v ~ 0.04
})
```

  The omega itself is untouched; only the prior is added.  Correlated
  omega priors work the same way, including the per row line form.  An
  `om.` name has to match a real between subject variability and never
  creates one.  `prior(om.eta.cl)` and `prior(eta.cl)` mean the same
  thing; the `om.` spelling exists so the shorthand has a name to put
  on the left of a `~`, since `eta.cl ~ ...` already means the omega
  value.

  Degrees of freedom and a normal prior are alternative ways of putting
  a prior on an omega, so a model that gives both is an error.

* Because normal priors are so common they also have a shorthand that
  reuses the matrix syntax: putting a *population estimate* on the left
  of a `~` gives it a normal prior with a zero mean and the given
  variance.

```r
lotri({
  tka <- 1
  tcl <- 3
  tv <- 4
  tka ~ 4          # tka ~ N(0, sd=2)
  tcl + tv ~ c(1,  # (tcl, tv) ~ MVN(0, Sigma)
               0.01, 1)
})
```

  All of the matrix spellings work here, including the per row line form
  (`tcl ~ 1; tv ~ c(0.01, 1)`) and the `sd()`, `var()`, `cor()`, `cov()`
  and `chol()` transformations.  The `<-` value remains the initial
  estimate; it is not the prior mean.  An uncorrelated block simply
  becomes independent normal priors.  A zero variance is an error.

  Note this changes behavior: `lotri({b <- 3; b ~ 0.4})` used to be a
  "duplicated parameter" error and is now a normal prior on `b`.  A name
  that is not an estimate still specifies an eta as before.

* Added `lotriPriorDists()` which returns the table of supported
  distributions (including the 'Stan' name for each), so that
  downstream packages can generate the corresponding 'Stan' code.

* The `lotriEst` data frame and `as.data.frame()` output gained a
  `prior` column.

* `prior()` also takes the normal prior shorthand, so
  `prior(tka) ~ 0.1` means what `tka ~ 0.1` means:

```r
lotri({
  tka <- 0.45
  prior(tka) ~ 0.1          # dnorm(0.45, sqrt(0.1))
  prior(tcl, tv) ~ c(1,     # multivariate, centered on the estimates
                     0.01, 1)
})
```

  Every matrix spelling works here as it does bare, including the per
  row line form, so a covariance can be built up a line at a time:

```r
lotri({
  tcl <- 3
  tv <- 4
  prior(tcl) ~ 1
  prior(tv) ~ c(0.001, 1)   # same 2x2 as `tcl ~ 1; tv ~ c(0.001, 1)`
})
```

  An uncorrelated group becomes independent normal priors, and the mean
  is what the model already says.  Note the line form is the one place a
  prior line is *not* order independent, since a row leans on the line
  before it.  The point is that a bare `~` cannot be used
  everywhere -- piping onto a model reads `tka ~ 0.1` as changing the
  estimate -- so the `prior()` flag gives the shorthand a spelling that
  works when piping too.

  A namespaced distribution such as `stats::dnorm(0, 1)` is now an
  error rather than being evaluated as a variance.

* A univariate prior (`dnorm()`, `dcauchy()`, ...) can now be put on a
  single omega COVARIANCE (off-diagonal) element, not just a diagonal one --
  a marginal, independent prior on that one cell, distinct from a whole-block
  distribution like `invWishart()`/`multiNormal()`:

```r
lotri({
  eta.cl + eta.v ~ c(0.1,
                     0.01, 0.2)
  prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
})
```

  The two names must covary with each other but need not be the model's
  *entire* connected block -- `prior(eta.cl, eta.v)` works even when a third,
  correlated eta is also in the block, unlike a whole-block prior which
  still requires naming every member. `prior(om.eta.cl, om.eta.v) ~ ...`
  is accepted identically. A block cannot carry both a whole-block
  `invWishart()`/`multiNormal()` prior and a marginal prior on one of its
  own cells -- these remain alternatives, not additions.

## Bug fixes

* A conditioned block written in the line form lost every row after the
  first.  `a ~ 1 | occ` did not set the `lastN` counter the line form
  accumulates through, so a following `b ~ c(0.1, 2) | occ` could not
  see the row before it and the block was silently truncated to 1x1.
  This showed up as `eval(as.expression(x))` returning a smaller matrix
  than `x` for any conditioned block of more than one parameter.

* `lotri()` read its own condition-property list with a partially-matching
  `attr()`.  Because `"lotri"` is a prefix of `"lotriLabels"`,
  `"lotriFix"` and friends, combining a conditioned matrix with a labelled
  one (as in `lotri(lotri(b ~ 2) | occ, lotri({c ~ 1; label("z")}))`)
  picked up the neighbouring attribute, attached it as the property list,
  and gave the result a spurious `lotri` class and a bogus `Properties:`
  line.  The internal reads are now exact.

* `lotri` now requires 'armadillo4r' 15.4.2 or newer.  The 'Armadillo'
  headers shipped with older 'armadillo4r' releases discard the return
  value of a `std::uniform_int_distribution` draw, which the 'libc++'
  shipped with 'clang' 23 now marks `[[nodiscard]]`.  That made `R CMD
  check` report a significant compilation warning -- and so a check
  `WARNING` -- on CRAN's clang-trunk flavour, even though none of
  `lotri`'s own sources were involved (#57).

* `src/nearPD.cpp` and `src/rcm.cpp` now include `<algorithm>`,
  `<cmath>` and `<cstddef>` for the `std::fill`/`std::fill_n`,
  `std::max`, `std::abs` and `std::size_t` they use directly, instead
  of relying on the 'Armadillo' headers to drag them in.  'libc++' has
  been steadily dropping transitive includes, and clang 23's is where
  that started breaking packages (#57).

* The `trace=TRUE` iteration message in `lotriNearPD()` printed an
  `arma::uword` with `%lld`; it now uses `%llu` with an explicit cast,
  so the format matches the argument on every 'Armadillo' word-size
  configuration (#57).

* The test suite no longer calls `structure()` with the deprecated
  special names `.Dim`, `.Dimnames` and `.Names`; it uses `dim`,
  `dimnames` and `names` instead.  This clears the "Found calls to
  structure() using deprecated special names" `NOTE` on the r-devel
  check flavours (#57).

* A `prior()` given for a fixed (`fix()`ed) parameter is now an error
  at resolution time, naming the parameter, instead of parsing and
  building cleanly and only surfacing as a problem in whatever
  consumes the priors.  A fixed parameter is a constant, so it cannot
  carry a prior; this applies to a univariate prior row, a member of a
  multivariate prior, and an omega-block member on a fixed omega
  element alike, and also to a block prior (`lkjCorr()`,
  `invWishart()`) whose block has a fixed covariance even if every
  variance in it is free.  The implicit `~invWishart(4)` shorthand,
  which applies to every omega block in the model, quietly skips a
  block that is entirely fixed instead (#52).
* `lotri()` can now re-parse the named `c()` form that
  `lotriAsExpression()`/`as.expression()` writes for a matrix above
  `nameEst`'s size threshold when a later row's names reach back into an
  earlier, already-closed block -- as happens for a combined theta+omega
  covariance matrix, where every omega row names all the preceding theta
  rows too. Previously an `om.`-prefixed row in that position was always
  misread as the `om.` normal prior shorthand instead of the next row of
  the matrix itself (#53).

* Labels now follow the matrix when `rcm=TRUE` re-orders it.
  Previously the labels stayed in the order they were parsed in while
  the matrix was permuted, so they were applied to the wrong
  parameters.

* `lotriLabels` are no longer dropped when matrices are combined (ie
  `lotri(mat1, mat2)` or `lotriMat()`); they are now concatenated in C
  along with the matrix itself.

* `as.expression()` now works on a `lotri` object that has only
  population estimates and no matrix; it used to fail with "second
  argument must be a list".

* Fixed rchk issues and small bugs found while linting

# lotri 1.0.4

* Fix unsigned integer underflow in `rcm.cpp`: when called with a
  0-row matrix, `n-1` would wrap to `UWORD_MAX` causing an infinite
  loop; added an early-return guard for `n == 0`.

* Fix type narrowing and underflow in `nearPD.cpp`: changed `unsigned
  int n` to `arma::uword` to prevent silent 32-bit truncation; added
  `n == 0` early-return guard to prevent out-of-bounds access on the
  eigenvalue vector.

* Fix variable shadowing in `lotriProp.c`: local `double val` renamed
  to `inVal` to eliminate `-Wshadow` warning against the function
  parameter.

* Fix hard-coded buffer size in `matlist.h`: `snprintf` buffer
  increased from 100 to 256 bytes and size passed via `sizeof(out)`.

* Use `R_xlen_t` instead of `int` for variables storing `Rf_length()`
  and `Rf_xlength()` returns across `matlist.h`, `lotriProp.c`,
  `lotriBounds.c`, and `lotriLstToMat.c`.

# lotri 1.0.3

* Lotri shifted to using `cpp4r` and `armadillo4r` (issue #41)

# lotri 1.0.2

* Fix for iov variables that are right next to one another (issue #37)

# lotri 1.0.1

* Now support labels on etas or covariances

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

* Added `lotriIsBlockMat()` to check to see if a matrix is in a block
  diagonal matrix form.

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
