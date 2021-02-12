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
  .cls <- class(.tmp)
  attr(.tmp, "lotriFix") <- NULL
  .w <- which(.cls == "lotriFix")
  .cls <- .cls[-.w]
  class(.tmp) <- NULL # Note that a matrix doesn't actually have a class
  print(.tmp)
  cat("this matrix has fixed elements\n")
}

##' @export
str.lotri <- function(object, ...) {
  str(object$.list)
}
