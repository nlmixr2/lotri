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

.as.lotri.data.frame.mat <- function(x) {
  x <- x[order(x$neta1, x$neta2), ]
  x$neta1 <- factor(paste(x$neta1))
  x$neta2 <- factor(paste(x$neta2), levels=levels(x$neta1))
  x$neta1 <- as.integer(x$neta1)
  x$neta2 <- as.integer(x$neta2)
  .r <- range(x$neta1)
  .neta1 <- .r[2] - .r[1] + 1
  .min <- .r[1] - 1
  .mat <- diag(.neta1)
  .matF <- matrix(FALSE, dim(.mat)[1], dim(.mat)[1])
  for (.i in seq_along(x$neta1)) {
    .mat[x$neta1[.i] - .min, x$neta2[.i] - .min] <- x$est[.i]
    .mat[x$neta2[.i] - .min, x$neta1[.i] - .min] <- x$est[.i]
    .matF[x$neta1[.i] - .min, x$neta2[.i] - .min] <- x$fix[.i]
    .matF[x$neta2[.i] - .min, x$neta1[.i] - .min] <- x$fix[.i]
  }
  .names <- vapply(seq_len(dim(.mat)[1]),
                   function(.i) {
                     x$name[x$neta1==.i & x$neta2 == .i]
                   }, character(1), USE.NAMES = FALSE)
  dimnames(.mat) <- list(.names, .names)
  dimnames(.matF) <- list(.names, .names)
  if(any(.matF)) {
    attr(.mat, "lotriFix") <- .matF
    class(.mat) <- c("lotriFix", class(.mat))
  }
  return(.mat)
}

##' @rdname as.lotri
##' @export
as.lotri.data.frame <- function(x, ..., default="") {
  ## Get lotriEst
  if (!all(c("name", "lower", "est", "upper", "fix", "label", "backTransform") %in% names(x))) {
    stop("the required names in the data.frame are not present; This needs:\n",
         "  name, lower, est, upper, fix, label, backTransform\n", call.=FALSE)
  }
  .lotriEst <- x[which(!is.na(x$ntheta)), c("name", "lower", "est", "upper", "fix", "label", "backTransform")]
  .lotriMatDf <- x[which(is.na(x$ntheta)), ]
  .cnd <- unique(.lotriMatDf$condition)
  if (length(.cnd) == 1) {
    .mat <- .as.lotri.data.frame.mat(.lotriMatDf)
  } else {
    .mat <- setNames(lapply(.cnd, function(.cur){
      .x <- .lotriMatDf[which(.lotriMatDf$condition == .cur), ]
      .as.lotri.data.frame.mat(.x)
    }), .cnd)
  }
  attr(.mat, "lotriEst") <- .lotriEst
  if (!inherits(.mat, "lotriFix")) class(.mat) <- c("lotriFix", class(.mat))
  .mat
}

##' @rdname as.lotri
##' @export
as.lotri.default <- function(x, ..., default = "") {
  if (inherits(x, "list") | inherits(x, "lotri")) {
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
  } else {
    stop("unsupported object of class c('", paste(class(x), collapse="', '"), "') used with `as.lotri`",
         call.=FALSE)
  }
}
