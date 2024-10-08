% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.expression.R
\name{lotriAsExpression}
\alias{lotriAsExpression}
\title{Change a matrix or lotri matrix to a lotri expression}
\usage{
lotriAsExpression(
  x,
  useIni = FALSE,
  plusNames = getOption("lotri.plusNames", FALSE),
  nameEst = getOption("lotri.nameEst", 5L)
)
}
\arguments{
\item{x}{matrix}

\item{useIni}{use the ini block}

\item{plusNames}{logical, when `TRUE` use the `a + b ~ c(1, 0.1,
1)` naming convention.  Otherwise use the lotri single line
convention `a ~ 1; b ~ c(0.1, 1)`}

\item{nameEst}{logical or integerish.  When logical `TRUE` will add
names to all matrix estimates and `TRUE` when using the lotri
single line convention i.e. `a~c(a=1); b~c(a=0.1, b=1)`.  When an
integer, the dimension of the matrix being displayed needs to
have a dimension above this number before names are displayed.}
}
\description{
Change a matrix or lotri matrix to a lotri expression
}
