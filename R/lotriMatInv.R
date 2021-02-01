#' Creates a logical matrix for block matrixes.
#'
#' @param mat Matrix
#' @param i Row/column where block matrix should be setup.
#'
#' @return A logical matrix returning where the elements should be
#'     zero.
#'
#' @keywords internal
#' @noRd
.lotriBlockZeros <- function(mat, i) {
  return(!((row(mat) > i & col(mat) > i) | (row(mat) <= i & col(mat) <= i)))
}

#' Return if diagonal i is a block matrix
#'
#' @param mat Input matrix
#' @param i index of rows
#' @return logical value
#' @author Matthew Fidler
#' @noRd
.lotriIsBlock <- function(mat, i) {
  return(all(mat[.lotriBlockZeros(mat, i)] == 0))
}

#' Converts a matrix into a list of block matrices
#'
#' @param mat Matrix to convert to a list of block matrices
#'
#' @return A list of block matrixes
#'
#' @details
#'
#' This is the inverse of `lotriMat()`
#'
#' @author Matthew Fidler
#' @examples
#'
#' # Create a block matrix using `lotri()`
#' mat <- lotri({
#'    a+b ~ c(1,
#'            0.5, 1)
#'    c ~ 1
#'    d +e ~ c(1,
#'             0.5, 1)
#' })
#'
#' print(mat)
#'
#' # now convert t a list of matrices
#'
#' mat2 <- lotriMatInv(mat)
#' print(mat2)
#'
#' # Of course you can convert it back to a full matrix:
#'
#' mat3 <- lotriMat(mat2)
#'
#' print(mat3)
#' @export
lotriMatInv <- function(mat) {
  .ret <- list()
  .mat <- mat
  .i <- 1
  .d <- dim(.mat)[1]
  while (.i < .d) {
    if (.lotriIsBlock(.mat, .i)) {
      .s <- seq_len(.i)

      .mat1 <- .mat[.s, .s, drop = FALSE]
      .ret <- c(.ret, list(.mat1))
      .mat <- .mat[-.s, -.s, drop = FALSE]
      .d <- dim(.mat)[1]
      .i <- 1
    } else {
      .i <- .i + 1
    }
  }
  if (.d > 0){
    .ret <- c(.ret, list(.mat))
  }
  .ret
}
