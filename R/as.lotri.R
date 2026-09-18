#' As lower triangular matrix
#'
#' @param x Matrix or other data frame
#'
#' @param ... Other factors
#'
#' @param default Is the default factor when no conditioning is
#'     implemented.
#'
#' @return Lower triangular matrix
#'
#' @author Matthew Fidler
#'
#' @export
as.lotri <- function(x, ..., default = "") {
  UseMethod("as.lotri")
}

#' @rdname as.lotri
#' @export
as.lotri.matrix <- function(x, ..., default = "") {
  .Call(`_asLotriMat`, x, list(...), default = default)
}

#' Split a `<base>:same:<master...>` condition string
#'
#' @param cnd character vector of condition strings
#' @return list with `base` (the condition with any `:same:` suffix
#'   removed) and `master` (a list of the master element's name(s), or
#'   `NULL` where the row is not a repeated one)
#' @noRd
#' @author Matthew L. Fidler
.lotriSplitSameCondition <- function(cnd) {
  .base <- sub(":same:.*$", "", cnd)
  .master <- lapply(seq_along(cnd), function(.i) {
    if (is.na(cnd[.i])) return(NULL)
    if (.base[.i] == cnd[.i]) return(NULL)
    strsplit(sub("^.*?:same:", "", cnd[.i]), ":", fixed=TRUE)[[1]]
  })
  list(base=.base, master=.master)
}

.as.lotri.data.frame.mat <- function(x) {
  x <- x[order(x$neta1, x$neta2), ]
  x$neta1 <- factor(paste(x$neta1), levels=paste(sort(unique(x$neta1))))
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
  x$label <- as.character(x$label)
  .labels <- vapply(seq_len(dim(.mat)[1]),
                    function(.i) {
                      x$label[x$neta1==.i & x$neta2 == .i]
                    }, character(1), USE.NAMES = FALSE)
  dimnames(.mat) <- list(.names, .names)
  dimnames(.matF) <- list(.names, .names)
  .hasLab <- FALSE
  if (!all(is.na(.labels))) {
    attr(.mat, "lotriLabels") <- .labels
    .hasLab <- TRUE
  }
  if (any(names(x) == "prior")) {
    x$prior <- as.character(x$prior)
    .priors <- vapply(seq_len(dim(.mat)[1]),
                      function(.i) {
                        x$prior[x$neta1 == .i & x$neta2 == .i]
                      }, character(1), USE.NAMES = FALSE)
    if (!all(is.na(.priors))) {
      attr(.mat, "lotriPriors") <- .priors
      .hasLab <- TRUE
    }
    ## an off-diagonal (covariance) row's own `name` is already the
    ## "(name_i,name_j)" key `.as.data.frame.lotriFix.mat()` builds, so it
    ## can be used verbatim as the `lotriOffDiagPriors` key
    .wOff <- which(x$neta1 != x$neta2 & !is.na(x$prior))
    if (length(.wOff) > 0L) {
      .priorsOffDiag <- setNames(x$prior[.wOff], x$name[.wOff])
      attr(.mat, "lotriOffDiagPriors") <- .priorsOffDiag
      .hasLab <- TRUE
    }
  }
  ## a declared eta distribution is diagonal only, so it rebuilds exactly
  ## like `lotriLabels` does
  if (any(names(x) == "etaDist")) {
    x$etaDist <- as.character(x$etaDist)
    .etaDists <- vapply(seq_len(dim(.mat)[1]),
                        function(.i) {
                          x$etaDist[x$neta1 == .i & x$neta2 == .i]
                        }, character(1), USE.NAMES = FALSE)
    if (!all(is.na(.etaDists))) {
      attr(.mat, "lotriEtaDists") <- .etaDists
      .hasLab <- TRUE
    }
  }
  ## rebuild the `same()` linkage from the condition column.  Offsets are
  ## derived from the DIAGONAL rows; an off diagonal row of a repeated
  ## block carries the same offset by construction.
  .sp <- .lotriSplitSameCondition(as.character(x$condition))
  .same <- rep(0L, dim(.mat)[1])
  .wd <- which(x$neta1 == x$neta2)
  for (.i in .wd) {
    .m <- .sp$master[[.i]]
    if (is.null(.m)) next
    if (length(.m) != 1L) {
      stop("a diagonal 'same()' condition names ", length(.m),
           " elements: '", x$condition[.i], "'", call.=FALSE)
    }
    .w <- which(.names == .m)
    if (length(.w) != 1L) {
      stop("the 'same()' condition '", x$condition[.i], "' refers to '",
           .m, "', which is ",
           ifelse(length(.w) == 0L, "not in", "ambiguous in"),
           " this block", call.=FALSE)
    }
    .cur <- x$neta1[.i]
    if (.w >= .cur) {
      stop("the 'same()' condition '", x$condition[.i],
           "' must refer to an earlier parameter", call.=FALSE)
    }
    .same[.cur] <- as.integer(.cur - .w)
  }
  ## a repeated element is not a parameter of its own, so it cannot
  ## carry a prior -- `lotri()` refuses one at parse time, and reading a
  ## frame that pairs the two would otherwise build an object that
  ## cannot be written back out
  if (any(names(x) == "prior")) {
    .wp <- which(!is.na(x$prior) & !is.null(.sp$master) &
                   vapply(.sp$master, function(.m) !is.null(.m),
                          logical(1), USE.NAMES=FALSE))
    if (length(.wp) > 0L) {
      stop("'", x$name[.wp[1]], "' repeats an earlier block with ",
           "'same()', so it cannot carry its own prior; put the prior ",
           "on the block it repeats", call.=FALSE)
    }
  }
  if (any(.same != 0L)) {
    ## master wins: an estimator writes back only the block being
    ## estimated, so a repeated block takes its values from its master
    ## rather than from whatever the frame happened to carry
    for (.i in seq_along(.same)) {
      .d <- .same[.i]
      if (.d == 0L) next
      for (.j in seq_len(dim(.mat)[1])) {
        .dj <- .same[.j]
        if (.dj != .d) next
        .mat[.i, .j] <- .mat[.i - .d, .j - .d]
        .matF[.i, .j] <- .matF[.i - .d, .j - .d]
      }
    }
    ## store the NORMALISED offsets, so the attribute matches the view
    ## every consumer reads: a chain naming the preceding copy (the way
    ## NONMEM's chained SAME reads) resolves to the original master
    .fmAttr <- attr(.mat, "lotriFix")
    attr(.mat, "lotriFix") <- .matF
    .fm <- .lotriSameFamilies(.mat, .same)
    attr(.mat, "lotriFix") <- .fmAttr
    if (!is.null(.fm)) {
      attr(.mat, "lotriSame") <- .fm$same
      .hasLab <- TRUE
    }
  }
  if (any(.matF) || .hasLab) {
    attr(.mat, "lotriFix") <- .matF
    class(.mat) <- c("lotriFix", class(.mat))
  }
  .mat
}

#' @rdname as.lotri
#' @export
as.lotri.data.frame <- function(x, ..., default="") {
  ## Get lotriEst
  if (!all(c("name", "lower", "est", "upper", "fix", "label", "backTransform") %in%
             names(x))) {
    stop("the required names in the data.frame are not present; This needs:\n",
         "  name, lower, est, upper, fix, label, backTransform\n", call.=FALSE)
  }
  ## `prior` is optional so that data frames created before priors were
  ## supported still convert
  if (!any(names(x) == "prior")) {
    x$prior <- rep(NA_character_, nrow(x))
  }
  .lotriEst <- x[which(!is.na(x$ntheta)), c("name", "lower", "est", "upper",
                                            "fix", "label", "backTransform",
                                            "prior")]
  .lotriMatDf <- x[which(is.na(x$ntheta)), ]
  ## group on the BASE condition: a repeated block's rows carry a
  ## `:same:` suffix, and splitting on the raw string would break one
  ## block into one "condition" per mirrored element
  .base <- .lotriSplitSameCondition(as.character(.lotriMatDf$condition))$base
  .cnd <- unique(.base)
  if (length(.cnd) == 1 && (is.na(.cnd) || .cnd == "id")) {
    ## the default level is returned bare, the way it always has been
    .mat <- .as.lotri.data.frame.mat(.lotriMatDf)
  } else if (length(.cnd) == 1) {
    ## but a single NON default level has to keep its name, or an
    ## occasion-only model comes back looking like an id level one
    .mat <- setNames(list(.as.lotri.data.frame.mat(.lotriMatDf)), .cnd)
  } else {
    .mat <- setNames(lapply(.cnd, function(.cur) {
      .x <- .lotriMatDf[which(.base == .cur), ]
      .as.lotri.data.frame.mat(.x)
    }), .cnd)
  }
  attr(.mat, "lotriEst") <- .lotriEst
  if (!inherits(.mat, "lotriFix")) class(.mat) <- c("lotriFix", class(.mat))
  .mat
}

#' @rdname as.lotri
#' @export
as.lotri.default <- function(x, ..., default = "") {
  if (inherits(x, "list") || inherits(x, "lotri")) {
    .ret <- x
    class(.ret) <- NULL
    .n <- names(.ret)
    .w <- which(names(.ret) == "")
    if (length(.w) == 1) {
      .n[.w] <- default
      names(.ret) <- .n
    }
    class(.ret) <- "lotri"
    .ret
  } else {
    stop("unsupported object of class c('", paste(class(x), collapse="', '"), "') used with `as.lotri`",
         call.=FALSE)
  }
}
