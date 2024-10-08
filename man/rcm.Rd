% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcm.R
\name{rcm}
\alias{rcm}
\title{Use the RCM algorithm to permute to banded matrix}
\usage{
rcm(x)
}
\arguments{
\item{x}{A symmetric matrix}
}
\value{
A permuted matrix that should be banded
}
\description{
The RCM stands for the reverse Cuthill McKee (RCM) algorithm
which is used to permute the matrix to a banded matrix.
}
\examples{

m <- lotri({
 a + b + c + d + e + f + g + h + i + j + k + l + m + n + o +
 p ~ c(0.4, 0, 0.3, 0, 0, 0, -0.1, 0, 0, 0.2, 0, 0, 0,
       0, 0.5, 0, 0, 0, 0, 0, 1.3, 0, 0, 0, 0, 0, -0.6, 0.8,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.2, 0, 0.3,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.1, 0.2, 0, 0, 0.2,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, -1.1,
       0.9, 0, 0, 0, 0, 0, 0, 0, 4.7, 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0.5, 0, 0.2, 0, 0, 0, 1.9)
})

rcm(m)

}
