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
