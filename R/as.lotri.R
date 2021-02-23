##' As lower triangular matrix
##'
##' @param x Matrix or other data frame
##'
##' @param ... Other factors
##'
##' @param default Is the default factor when no conditioning is
##'     implemented.
##'
##' @return Lower triangular matrix
##'
##' @author Matthew Fidler
##'
##' @export
as.lotri <- function(x, ..., default = "") {
  UseMethod("as.lotri")
}

##' @rdname as.lotri
##' @export
as.lotri.matrix <- function(x, ..., default = "") {
  .Call(`_asLotriMat`, x, list(...), default = default)
}

##' @rdname as.lotri
##' @export
as.lotri.default <- function(x, ..., default = "") {
  .ret <- x
  class(.ret) <- NULL
  .n <- names(.ret)
  .w <- which(names(.ret) == "")
  if (length(.w) == 1) {
    .n[.w] <- default
    names(.ret) <- .n
  }
  class(.ret) <- "lotri"
  return(.ret)
}
