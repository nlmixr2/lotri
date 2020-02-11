# lotri
[![Travis build status](https://travis-ci.org/nlmixrdevelopment/lotri.svg?branch=master)](https://travis-ci.org/nlmixrdevelopment/lotri) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/nlmixrdevelopment/lotri?branch=master&svg=true)](https://ci.appveyor.com/project/nlmixrdevelopment/lotri) [![Coverage status](https://codecov.io/gh/nlmixrdevelopment/lotri/branch/master/graph/badge.svg)](https://codecov.io/github/nlmixrdevelopment/lotri?branch=master) [![CRAN status](https://www.r-pkg.org/badges/version/lotri)](https://cran.r-project.org/package=lotri)

Easily Specify block-diagonal matrices with (lo)wer (tri)angular
matrices.  Its as if you have won the (badly spelled) lotri (or lottery).

This was made to allow people (like me) to specify lower triangular
matrices similar to the domain specific language implemented in
`nlmixr`.  Originally I had it included in `RxODE`, but thought it may
have more general applicability, so I separated it into a new
package. 

For me, specifying the matricies in this way is easier than
specifying them using R's default matrix.  For instance to fully
specify a simple `2x2` matrix, in R you specify:

```{r}
mat <- matrix(c(1, 0.5, 0.5, 1),nrow=2,ncol=2,dimnames=list(c("a", "b"), c("a", "b")))
```

With `lotri`, you simply specify:

```{r}
library(lotri)

mat <- lotri(a+b ~ c(1,
                     0.5, 1))
```

I find it more legible and easier to specify, especially if you have a
more complex matrix.  For instance with the more complex matrix:

```{r}
mat <- lotri({
    a+b ~ c(1,
            0.5, 1)
    c ~ 1
    d +e ~ c(1,
             0.5, 1)
})
```

To fully specify this in base R you would need to use:

```{r}
mat <- matrix(c(1, 0.5, 0, 0, 0,
                0.5, 1, 0, 0, 0,
                0, 0, 1, 0, 0,
                0, 0, 0, 1, 0.5,
                0, 0, 0, 0.5, 1),
              nrow=5, ncol=5,
              dimnames= list(c("a", "b", "c", "d", "e"), c("a", "b", "c", "d", "e")))
```

Of course with the excellent `Matrix` package this is a bit easier:

```{r}
library(Matrix)
mat <- matrix(c(1, 0.5, 0.5, 1),nrow=2,ncol=2,dimnames=list(c("a", "b"), c("a", "b")))
mat <- bdiag(list(mat, matrix(1), mat))
## Convert back to standard matrix
mat <- as.matrix(mat)
##
dimnames(mat) <- list(c("a", "b", "c", "d", "e"), c("a", "b", "c", "d", "e"))
```

Regardless, I think `lotri` is a bit easier to use.

## New features

A new feature is the ability to condition on variables by `|`.  This
will be useful when simulating nested random effects using the
upcoming `RxODE`

