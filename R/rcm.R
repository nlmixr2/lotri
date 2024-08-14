#' Use the RCM algorithm to permute to banded matrix
#'
#' The RCM stands for the reverse Cuthill McKee (RCM) algorithm
#' which is used to permute the matrix to a banded matrix.
#'
#' @param x A symmetric matrix
#' @return A permuted matrix that should be banded
#' @export
#' @examples
#'
#' m <- lotri({
#'  a + b + c + d + e + f + g + h + i + j + k + l + m + n + o +
#'  p ~ c(0.4, 0, 0.3, 0, 0, 0, -0.1, 0, 0, 0.2, 0, 0, 0,
#'        0, 0.5, 0, 0, 0, 0, 0, 1.3, 0, 0, 0, 0, 0, -0.6, 0.8,
#'        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2,
#'        0, 0, 0, 0, 0, 0, 0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0, 0,
#'        0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.2, 0, 0.3,
#'        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.1, 0.2, 0, 0, 0.2,
#'        0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, -1.1,
#'        0.9, 0, 0, 0, 0, 0, 0, 0, 4.7, 0, 0, 0, 0, 0, 0, 0, 0,
#'        0, 0.5, 0, 0.2, 0, 0, 0, 1.9)
#' })
#'
#' rcm(m)
#'
rcm <- function(x) {
  checkmate::assertMatrix(x, mode="double",
                          any.missing=FALSE, row.names="unique")
  .d <- dimnames(x)
  if (!identical(.d[[1]], .d[[2]])) {
    stop("The matrix must be square, symmetric with matching row and column names")
  }
  .Call(`_lotri_rcm_`, x)
}
