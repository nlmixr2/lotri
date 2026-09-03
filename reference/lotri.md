# Easily Specify block-diagonal matrices with lower triangular info

Easily Specify block-diagonal matrices with lower triangular info

## Usage

``` r
lotri(x, ..., cov = FALSE, rcm = FALSE, envir = parent.frame(), default = "id")
```

## Arguments

- x:

  list, matrix or expression, see details

- ...:

  Other arguments treated as a list that will be concatenated then
  reapplied to this function.

- cov:

  either a boolean or a function accepting a matrix input.

  When a boolean, \`cov\` describes if this matrix definition is
  actually a rxode2/nlmixr2-style covariance matrix. If so, \`lotri()\`
  will enforce certain regularity conditions:

  \- When diagonal elements are zero, the off-diagonal elements are
  zero. This means the covariance element is fixed to zero and not truly
  part of the covariance matrix in general.

  \- For the rest of the matrix, \`lotri\` will check that it is
  non-positive definite (which is required for covariance matrix in
  general)

  It is sometimes difficult to adjust covariance matrices to be
  non-positive definite. For this reason \`cov\` may also be a function
  accepting a matrix input and returning a non-positive definite matrix
  from this matrix input. When this is a function, it is equivalent to
  \`cov=TRUE\` with the additional ability to correct the matrix to be
  non-positive definite if needed.

- rcm:

  logical; if \`TRUE\`, the matrix will be reordered to change the
  matrix to a banded matrix, which is easier to express in \`lotri\`
  than a full matrix. The RCM stands for the reverse Cuthill McKee (RCM)
  algorithm which is used for this matrix permutation. (see \`rcm()\`)

- envir:

  the [`environment`](https://rdrr.io/r/base/environment.html) in which
  `expr` is to be evaluated. May also be `NULL`, a list, a data frame, a
  pairlist or an integer as specified to
  [`sys.call`](https://rdrr.io/r/base/sys.parent.html).

- default:

  Is the default factor when no conditioning is implemented.

## Value

named symmetric matrix useful in \`rxode2()\` simulations (and perhaps
elsewhere)

## Details

This can take an R matrix, a list including matrices or expressions, or
expressions

Expressions can take the form

name ~ estimate

Or the lower triangular matrix when "adding" the names

name1 + name2 ~ c(est1, est2, est3)

The matrices are concatenated into a block diagonal matrix, like
[`bdiag`](https://rdrr.io/pkg/Matrix/man/bdiag.html), but allows
expressions to specify matrices easier.

A block can be repeated, sharing one set of estimates, with

name3 + name4 ~ same()

This is NONMEM's `$OMEGA BLOCK(n) SAME`, and it is how an inter-occasion
variability block is written when every occasion draws its own random
effects from one shared covariance. `same()` repeats the immediately
preceding \*block\* under new names; a further `same()` repeats that
same original block rather than the copy, the way NONMEM chains `SAME`.
It takes no arguments, may be used with a condition
(`name3 + name4 ~ same() | occ`), and inherits the fixed flags of the
block it repeats.

A prior cannot be put on a repeated block: it is not a parameter of its
own, it is the block it mirrors, so the prior goes on that block.

`same()` looks back only within one
[`{}`](https://rdrr.io/r/base/Paren.html) block, and only at its own
level of variability – though other levels may be written in between.
Each extra argument to `lotri()` is parsed by its own call, so
`lotri(a + b ~ c(1, 0.1, 2), c1 + d1 ~ same())` has nothing to repeat;
write the two lines in one `lotri({})` block instead.

Note that the rows of one block always share a level of variability,
because they covary. Writing the line form with a condition on a later
row therefore places the whole block at that level:
`lotri({a ~ 1; b ~ c(0.1, 2) | occ})` puts both `a` and `b` in `occ`.

In the data frame from
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) the
repetition is recorded in the existing `condition` column rather than in
a new column, naming the element that is mirrored: `"id:same:name1"` on
a diagonal row and `"id:same:name1:name2"` on a covariance row. Use
[`lotriBaseCondition`](https://nlmixr2.github.io/lotri/reference/lotriBaseCondition.md)
and its companions to read that column; comparing it directly
(`condition == "id"`) will misclassify a repeated block.

This is distinct from the condition property `cnd(same = n)`, which
repeats a whole nesting level rather than one block, and which
[`lotriSep`](https://nlmixr2.github.io/lotri/reference/lotriSep.md)
uses. The two compose.

Population estimates can be given with

name \<- estimate

or with bounds, `name <- c(lower, estimate, upper)`

A prior distribution can be put on any of these with

prior(name) ~ dist(...)

Since the statement names what it applies to, prior lines can be put
anywhere in the block. A prior can be given for a population estimate,
for a single eta, or for a whole covariance block:

prior(eta1, eta2) ~ lkjCorr(2)

Normal priors also have a shorthand that reuses the matrix syntax: when
the name on the left of a `~` is a population estimate (instead of an
eta), it is a normal prior with a zero mean and the given variance

tka ~ 4

tcl + tv ~ c(1, 0.01, 1)

The first is a normal prior on `tka` with a standard deviation of 2 and
the second a multivariate normal prior on `tcl` and `tv` with a zero
mean vector. Every matrix spelling works, including the per row line
form and the
[`sd()`](https://rdrr.io/r/stats/sd.html)/[`cor()`](https://rdrr.io/r/stats/cor.html)
transformations. The estimate given with `<-` stays the initial
estimate; it is not the prior mean.

The distributions understood are listed by
[`lotriPriorDists`](https://nlmixr2.github.io/lotri/reference/lotriPriorDists.md).
Each has three accepted spellings: the R name where R parameterizes it
the same way 'Stan' does
([`dnorm()`](https://rdrr.io/r/stats/Normal.html)), the camelCase name
(`invWishart()`), and the 'Stan' name (`inv_wishart()`). The canonical
one is the R name where there is a faithful one and the camelCase name
otherwise.

Bounds are not repeated in the prior; a parameter declared as `c(0, 1)`
with a `dcauchy(0, 5)` prior is a half-Cauchy.

The scale matrix of the Wishart family is optional, since the block it
is put on already is that matrix, so `prior(eta1, eta2) ~ invWishart(4)`
gives just the degrees of freedom (the `$OMEGAPD` of a NONMEM NWPRI
model).

## Author

Matthew L Fidler

## Examples

``` r

## A few ways to specify the same matrix
lotri({et2 + et3 + et4 ~ c(40,
                           0.1, 20,
                           0.1, 0.1, 30)})
#>      et2  et3  et4
#> et2 40.0  0.1  0.1
#> et3  0.1 20.0  0.1
#> et4  0.1  0.1 30.0

## You  do not need to enclose in {}
lotri(et2 + et3 + et4 ~ c(40,
                          0.1, 20,
                          0.1, 0.1, 30),
          et5 ~ 6)
#>      et2  et3  et4 et5
#> et2 40.0  0.1  0.1   0
#> et3  0.1 20.0  0.1   0
#> et4  0.1  0.1 30.0   0
#> et5  0.0  0.0  0.0   6
## But if you do enclose in {}, you can use
## multi-line matrix specifications:

lotri({et2 + et3 + et4 ~ c(40,
                           0.1, 20,
                           0.1, 0.1, 30)
          et5 ~ 6
          })
#>      et2  et3  et4 et5
#> et2 40.0  0.1  0.1   0
#> et3  0.1 20.0  0.1   0
#> et4  0.1  0.1 30.0   0
#> et5  0.0  0.0  0.0   6

## A block can be repeated with `same()`, which is NONMEM's
## `$OMEGA BLOCK(n) SAME`: one estimated 2x2 shared by three blocks,
## the usual shape for correlated inter-occasion variability

iov <- lotri({
  iov.cl1 + iov.v1 ~ c(0.1,
                       0.01, 0.2)
  iov.cl2 + iov.v2 ~ same()
  iov.cl3 + iov.v3 ~ same()
})

iov
#>         iov.cl1 iov.v1 iov.cl2 iov.v2 iov.cl3 iov.v3
#> iov.cl1    0.10   0.01    0.00   0.00    0.00   0.00
#> iov.v1     0.01   0.20    0.00   0.00    0.00   0.00
#> iov.cl2    0.00   0.00    0.10   0.01    0.00   0.00
#> iov.v2     0.00   0.00    0.01   0.20    0.00   0.00
#> iov.cl3    0.00   0.00    0.00   0.00    0.10   0.01
#> iov.v3     0.00   0.00    0.00   0.00    0.01   0.20
#> 
#> This matrix repeats blocks with `same()`:
#>   iov.cl2, iov.v2 repeat iov.cl1, iov.v1
#>   iov.cl3, iov.v3 repeat iov.cl1, iov.v1
#> 

## the repetition rides in the `condition` column, so no column is
## added to the data frame

as.data.frame(iov)$condition
#> [1] "id"                     "id"                     "id"                    
#> [4] "id:same:iov.cl1"        "id:same:iov.cl1:iov.v1" "id:same:iov.v1"        
#> [7] "id:same:iov.cl1"        "id:same:iov.cl1:iov.v1" "id:same:iov.v1"        

## You can also add lists or actual R matrices as in this example:
lotri(list(et2 + et3 + et4 ~ c(40,
                               0.1, 20,
                               0.1, 0.1, 30),
              matrix(1,dimnames=list("et5","et5"))))
#>      et2  et3  et4 et5
#> et2 40.0  0.1  0.1   0
#> et3  0.1 20.0  0.1   0
#> et4  0.1  0.1 30.0   0
#> et5  0.0  0.0  0.0   1

## Overall this is a flexible way to specify symmetric block
## diagonal matrices.

## For rxode2, you may also condition based on different levels of
## nesting with lotri;  Here is an example:

mat <- lotri(lotri(iov.Ka ~ 0.5,
                    iov.Cl ~ 0.6),
              lotri(occ.Ka ~ 0.5,
                    occ.Cl ~ 0.6) | occ(lower=4,nu=3))

mat
#> [[1]]
#>        iov.Ka iov.Cl
#> iov.Ka    0.5    0.0
#> iov.Cl    0.0    0.6
#> 
#> $occ
#>        occ.Ka occ.Cl
#> occ.Ka    0.5    0.0
#> occ.Cl    0.0    0.6
#> 
#> Properties: lower, nu 

## you may access features of the matrix simply by `$` that is

mat$lower # Shows the lower bound for each condition
#> [[1]]
#> iov.Ka iov.Cl 
#>   -Inf   -Inf 
#> 
#> $occ
#> occ.Ka occ.Cl 
#>      4      4 
#> 

mat$lower$occ # shows the lower bound for the occasion variable
#> occ.Ka occ.Cl 
#>      4      4 

## Note that `lower` fills in defaults for parameters.  This is true
## for `upper` true;  In fact when accessing this the defaults
## are put into the list

mat$upper
#> [[1]]
#> numeric(0)
#> 
#> $occ
#> occ.Ka occ.Cl 
#>    Inf    Inf 
#> 

## However all other values return NULL if they are not present like

mat$lotri
#> NULL

## And values that are specified once are only returned on one list:

mat$nu
#> $occ
#> [1] 3
#> 

mat$nu$occ
#> [1] 3
mat$nu$id
#> NULL

## You can also change the default condition with `as.lotri`

mat <- as.lotri(mat, default="id")

mat
#> $id
#>        iov.Ka iov.Cl
#> iov.Ka    0.5    0.0
#> iov.Cl    0.0    0.6
#> 
#> $occ
#>        occ.Ka occ.Cl
#> occ.Ka    0.5    0.0
#> occ.Cl    0.0    0.6
#> 
#> Properties: lower, nu 
```
