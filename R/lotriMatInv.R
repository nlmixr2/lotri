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
  if (!inherits(mat, "matrix"))
    stop("'mat' must be a matrix", call.=FALSE)
  if (!is.null(attr(mat, "lotriEst"))) {
    stop("a lotri matrix with attached estimates cannot be converted to a list matrix\n",
         "drop with `lotriEst(x,drop=TRUE)",
         call.=FALSE)
  }
  .matF <- attr(mat, "lotriFix")
  .matU <- attr(mat, "lotriUnfix")
  .ret <- list()
  .mat <- mat
  .i <- 1
  .d <- dim(.mat)[1]
  while (.i < .d) {
    if (.lotriIsBlock(.mat, .i)) {
      .s <- seq_len(.i)
      .mat1 <- .mat[.s, .s, drop = FALSE]
      if (!is.null(.matF)) {
        .mat1F <- .matF[.s, .s, drop = FALSE]
        attr(.mat1, "lotriFix") <- .mat1F
        class(.mat1) <- c("lotriFix", class(.mat1))
        .matF <- .matF[-.s, -.s, drop = FALSE]
      } else if (!is.null(.matU)) {
        .mat1U <- .matU[.s, .s, drop = FALSE]
        attr(.mat1, "lotriUnfix") <- .mat1F
        class(.mat1) <- c("lotriFix", class(.mat1))
        .matU <- .matU[-.s, -.s, drop = FALSE]
      }
      .ret <- c(.ret, list(.mat1))
      .mat <- .mat[-.s, -.s, drop = FALSE]
      .d <- dim(.mat)[1]
      .i <- 1
    } else {
      .i <- .i + 1
    }
  }
  if (.d > 0){
    if (!is.null(.matF)) {
      attr(.mat, "lotriFix") <- .matF
      class(.mat) <- c("lotriFix", class(.mat))
    } else if (!is.null(.matU)) {
      attr(.mat, "lotriUnfix") <- .matU
      class(.mat) <- c("lotriFix", class(.mat))
    }
    .ret <- c(.ret, list(.mat))
  }
  .ret
}
#' Determine if the matrix is a block matrix
#'
#' @param mat matrix to determine if it is a block matrix
#' @return logical value, TRUE if it is a block matrix and FALSE
#'   otherwise
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' m <- lotri({
#'   a ~ c(a = 0.4)
#'   b ~ c(a = 0, b = 0.3)
#'   c ~ c(a = 0, b = 0, c = 0)
#'   d ~ c(a = -0.1, b = 0, c = 0, d = 0.2)
#'   e ~ c(a = 0, b = 0, c = 0, d = 0, e = 0.5)
#'   f ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 1.3)
#'   g ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = -0.6, g = 0.8)
#'   h ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0)
#'   i ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
#'         i = 0.2)
#'   j ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
#'         i = 0, j = 0.9)
#'   k ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
#'         i = 0, j = 0, k = 0.9)
#'   l ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
#'         i = 0, j = -0.2, k = 0, l = 0.3)
#'   m ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
#'         i = 0, j = 0, k = 0, l = 0, m = 2.1)
#'   n ~ c(a = 0.2, b = 0, c = 0, d = 0.2, e = 0, f = 0, g = 0,
#'         h = 0, i = 0, j = 0, k = 0, l = 0, m = 0, n = 0.4)
#'   o ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = -1.1, g = 0.9,
#'         h = 0, i = 0, j = 0, k = 0, l = 0, m = 0, n = 0, o = 4.7)
#'   p ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
#'         i = 0, j = 0.5, k = 0, l = 0.2, m = 0, n = 0, o = 0,
#'        p = 1.9)
#' })
#'
#' lotriIsBlockMat(m)
#'
#' lotriIsBlockMat(rcm(m))
#'
lotriIsBlockMat <- function(mat) {
  .lst <- lotriMatInv(mat)
  all(vapply(seq_along(.lst), function (i) {
    .mat <- .lst[[i]]
    if (dim(.mat)[1] == 1) {
      return(TRUE)
    }
    !any(.mat[lower.tri(.mat, diag = FALSE)] == 0)
  }, logical(1), USE.NAMES = FALSE))
}
