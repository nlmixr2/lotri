#' C++ implementation of Matrix's nearPD
#'
#' With `ensureSymmetry` it makes sure it is symmetric by applying 0.5*(t(x) + x) before using lotriNearPD
#'
#' @inherit Matrix::nearPD
#'
#' @param ensureSymmetry  logical; by default, \code{\link[Matrix]{symmpart}(x)}
#' is used whenever \code{isSymmetric(x)} is not true.  The user
#' can explicitly set this to \code{TRUE} or \code{FALSE}, saving the
#' symmetry test. \emph{Beware} however that setting it \code{FALSE}
#' for an \bold{a}symmetric input \code{x}, is typically nonsense!
#'
#' @param do2eigen logical indicating if a `posdefify()` (like in the
#'   package `sfsmisc`) eigen step should be applied to the result of
#'   the Higham algorithm
#'
#' @seealso A first version of this (with non-optional \code{corr=TRUE})
#' has been available as `sfsmisc::nearcor()` and
#' more simple versions with a similar purpose
#' `sfsmisc::posdefify()`
#'
#' @return unlike the matrix package, this simply returns the nearest
#'   positive definite matrix
#'
#' @author Jens \enc{Oehlschlägel}{Oehlschlaegel} donated a first
#'   version to Matrix.  Subsequent changes by the Matrix package
#'   authors, later modifications to C++ by Matthew Fidler
#'
#' @details
#'
#' This implements the algorithm of Higham (2002), and then (if
#' \code{do2eigen} is true) forces positive definiteness using code from
#' `sfsmisc::posdefify()`.  The algorithm of Knol and ten
#' Berge (1989) (not implemented here) is more general in that it
#' allows constraints to (1) fix some rows (and columns) of the matrix and
#' (2) force the smallest eigenvalue to have a certain value.
#'
#' Note that setting \code{corr = TRUE} just sets \code{diag(.) <- 1}
#' within the algorithm.
#'
#' Higham (2002) uses Dykstra's correction, but the version by Jens
#'  \enc{Oehlschlägel}{Oehlschlaegel} did not use it (accidentally),
#'   and still gave reasonable results; this simplification, now only
#'  used if \code{doDykstra = FALSE},
#'   was active in \code{nearPD()} up to Matrix version 0.999375-40.
#'
#' @examples
#'
#' set.seed(27)
#' m <- matrix(round(rnorm(25),2), 5, 5)
#' m <- m + t(m)
#' diag(m) <- pmax(0, diag(m)) + 1
#' (m <- round(cov2cor(m), 2))
#'
#' near.m <- lotriNearPD(m)
#' round(near.m, 2)
#' norm(m - near.m) # 1.102 / 1.08
#'
#' round(lotriNearPD(m, only.values=TRUE), 9)
#'
#' ## A longer example, extended from Jens' original,
#' ## showing the effects of some of the options:
#'
#' pr <- matrix(c(1,     0.477, 0.644, 0.478, 0.651, 0.826,
#'                0.477, 1,     0.516, 0.233, 0.682, 0.75,
#'                0.644, 0.516, 1,     0.599, 0.581, 0.742,
#'                0.478, 0.233, 0.599, 1,     0.741, 0.8,
#'                0.651, 0.682, 0.581, 0.741, 1,     0.798,
#'                0.826, 0.75,  0.742, 0.8,   0.798, 1),
#'                nrow = 6, ncol = 6)
#'
#' nc  <- lotriNearPD(pr)
#'
#' @export
lotriNearPD <- function(x, keepDiag = FALSE, do2eigen = TRUE, doDykstra = TRUE, only.values = FALSE, ensureSymmetry=!isSymmetric(x), eig.tol = 1e-6, conv.tol = 1e-7, posd.tol = 1e-8, maxit = 100L,
                     trace = FALSE # nolint
                     ) {
  if (ensureSymmetry) {
    x <- 0.5 * (t(x) + x)
  }
  .Call(`_lotriNearPD_`, x, keepDiag, do2eigen, doDykstra, only.values, eig.tol, conv.tol, posd.tol, maxit,
        trace # nolint
        )
}
