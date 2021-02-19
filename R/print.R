##' @export
print.lotri <- function(x, ...) {
  .tmp <- x
  .lotri <- attr(.tmp, "lotri")
  class(.tmp) <- NULL
  attr(.tmp, "lotri") <- NULL
  print(.tmp)
  .names <- x$.names
  if (length(.names) > 0) {
    cat(paste0("Properties: ", paste(.names, collapse = ", ")), "\n")
  }
  return(invisible(x))
}

##' @export
print.lotriFix <- function(x, ...) {
  .tmp <- x
  .dim <- dim(x)
  .cls <- class(.tmp)
  .lotriFix <- attr(.tmp, "lotriFix")
  .lotriEst <- attr(.tmp, "lotriEst")
  if (all(.dim == 0L) & !is.null(.lotriEst)) {
    cat("Lotri Estimates (get with `lotriEst()`):\n")
    print(.lotriEst)
    return(invisible(x))
  }
  attr(.tmp, "lotriFix") <- NULL
  attr(.tmp, "lotriEst") <- NULL
  .w <- which(.cls == "lotriFix")
  .cls <- .cls[-.w]
  class(.tmp) <- NULL # Note that a matrix doesn't actually have a class
  if (!is.null(.lotriEst)) {
    cat("Lotri Estimates (get with `lotriEst()`):\n")
    print(.lotriEst)
    cat("\nMatrix:\n")
  }
  print(.tmp)
  if (!is.null(.lotriFix)) {
    cat("this matrix has fixed elements\n")
  }
  return(invisible(x))
}

##' @export
str.lotri <- function(object, ...) {
  str(object$.list)
}
