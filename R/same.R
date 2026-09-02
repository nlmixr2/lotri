#' Validated view of a matrix's `same()` repetitions
#'
#' The `lotriSame` offsets are relative, which is what makes
#' `lotriMatInv()` slicing and `lotriMat()` concatenation free, but it
#' means a list of blocks that has been reordered or trimmed can leave an
#' offset pointing at something it never described.  Every consumer --
#' `as.data.frame()`, `as.expression()` and `print()` -- has to agree on
#' which offsets still mean something, so they all come through here
#' rather than each grouping the vector its own way.
#'
#' A family is a *block*, not a row run: rows are walked in chunks of the
#' offset, so two repeated blocks that ended up adjacent do not fuse, and
#' a block stacked on a copy is rejected rather than being read as a copy
#' of a copy (which `same()` cannot re-parse).
#'
#' @param mat the whole matrix for one condition
#' @param same its `lotriSame` attribute
#' @return `NULL` when nothing survives, otherwise a list with `same`
#'   (the cleaned offset vector) and `families` (a list of
#'   `list(master=, copy=, d=)` index ranges)
#' @noRd
#' @author Matthew L. Fidler
.lotriSameFamilies <- function(mat, same) {
  if (is.null(same)) return(NULL)
  .n <- dim(mat)[1]
  if (length(same) != .n) return(NULL)
  .out <- as.integer(same)
  .out[is.na(.out)] <- 0L
  ## an offset pointing before the first row cannot describe anything
  .out[seq_len(.n) - .out < 1L] <- 0L
  .fam <- list()
  .i <- 1L
  while (.i <= .n) {
    .d <- .out[.i]
    if (.d == 0L) {
      .i <- .i + 1L
      next
    }
    ## the run of rows carrying this offset ...
    .j <- .i
    while (.j < .n && .out[.j + 1L] == .d) .j <- .j + 1L
    ## ... taken a block at a time.  The width is not simply `.d`: a run
    ## of one offset can be one wide block, or several narrow ones whose
    ## masters happen to sit that far back.  The widest valid reading is
    ## taken, which keeps a genuine family from being thrown away
    ## because a bogus one landed next to it in the run.
    .k <- .i
    while (.k <= .j) {
      .wid <- 0L
      .max <- min(.d, .j - .k + 1L)
      for (.try in seq(.max, 1L)) {
        if (.lotriSameOkFamily(mat, .out, .k, .try, .d, .n)) {
          .wid <- .try
          break
        }
      }
      if (.wid == 0L) {
        .out[.k] <- 0L
        .k <- .k + 1L
        next
      }
      .w <- .k:(.k + .wid - 1L)
      .fam[[length(.fam) + 1L]] <- list(master=.w - .d, copy=.w, d=.d)
      .k <- .k + .wid
    }
    .i <- .j + 1L
  }
  if (length(.fam) == 0L) return(NULL)
  list(same=.out, families=.fam)
}

#' Is one candidate repeated block valid?
#'
#' @param mat the whole matrix
#' @param out the offsets decided so far -- earlier rows are already
#'   settled, which is what makes "the master is a real master"
#'   answerable
#' @param k first row of the candidate copy
#' @param wid candidate width
#' @param d the offset
#' @param n `dim(mat)[1]`
#' @return TRUE when this really describes a repetition that can be
#'   written back out
#' @noRd
#' @author Matthew L. Fidler
.lotriSameOkFamily <- function(mat, out, k, wid, d, n) {
  .w <- k:(k + wid - 1L)
  .mw <- .w - d
  if (.mw[1] < 1L) return(FALSE)
  if (!all(out[.w] == d)) return(FALSE)
  ## the master must be a real master, not itself a copy
  if (!all(out[.mw] == 0L)) return(FALSE)
  for (.r in list(.w, .mw)) {
    ## the range must be separated from the rest of the matrix: one that
    ## covaries outside itself is not a block `same()` could have
    ## declared
    .o <- setdiff(seq_len(n), .r)
    if (length(.o) > 0L && !all(mat[.r, .o] == 0)) return(FALSE)
    ## and the boundary this range forces must not cut a covariance that
    ## spans it, or `.lotriSameSplit()` would drop that value entirely
    .before <- seq_len(min(.r) - 1L)
    .after <- if (max(.r) < n) seq(max(.r) + 1L, n) else integer(0)
    if (length(.before) > 0L && length(.after) > 0L &&
          !all(mat[.before, .after] == 0)) {
      return(FALSE)
    }
  }
  ## and every cell must really equal the cell it claims to repeat
  all(vapply(.w, function(.a) {
    all(vapply(.w, function(.b) {
      isTRUE(all.equal(mat[.a, .b], mat[.a - d, .b - d], tolerance=0))
    }, logical(1), USE.NAMES=FALSE))
  }, logical(1), USE.NAMES=FALSE))
}

#' Cleaned `lotriSame` offsets for a matrix
#'
#' @param mat the whole matrix for one condition
#' @param same its `lotriSame` attribute
#' @return cleaned integer vector, or `NULL`
#' @noRd
#' @author Matthew L. Fidler
.lotriSameClean <- function(mat, same) {
  .f <- .lotriSameFamilies(mat, same)
  if (is.null(.f)) return(NULL)
  .f$same
}

#' Slice one index range out of a matrix, carrying its attributes
#'
#' The same rules `lotriMatInv()` uses, but for an arbitrary range, so
#' that a declared block which connectivity would split (a covariance of
#' exactly zero) can be kept whole.
#'
#' @param mat matrix to slice
#' @param idx integer indexes to keep
#' @return the sliced matrix, `lotriFix` classed when it carries anything
#' @noRd
#' @author Matthew L. Fidler
.lotriSliceBlock <- function(mat, idx) {
  .m1 <- unclass(mat)[idx, idx, drop=FALSE]
  .cls <- FALSE
  for (.a in c("lotriFix", "lotriUnfix")) {
    .v <- attr(mat, .a)
    if (!is.null(.v)) {
      attr(.m1, .a) <- .v[idx, idx, drop=FALSE]
      .cls <- TRUE
    }
  }
  for (.a in c("lotriLabels", "lotriPriors")) {
    .v <- attr(mat, .a)
    if (!is.null(.v)) {
      attr(.m1, .a) <- .v[idx]
      .cls <- TRUE
    }
  }
  .v <- attr(mat, "lotriSame")
  if (!is.null(.v)) {
    .s <- .v[idx]
    if (any(.s != 0L)) {
      attr(.m1, "lotriSame") <- .s
      .cls <- TRUE
    }
  }
  .off <- attr(mat, "lotriOffDiagPriors")
  if (!is.null(.off) && length(.off) > 0L) {
    .dn <- dimnames(.m1)[[1]]
    .in <- vapply(names(.off), function(.k) {
      all(.lotriCovPriorKeyNames(.k) %in% .dn)
    }, logical(1), USE.NAMES=FALSE)
    if (any(.in)) {
      attr(.m1, "lotriOffDiagPriors") <- .off[.in]
      .cls <- TRUE
    }
  }
  if (.cls) class(.m1) <- c("lotriFix", class(.m1))
  .m1
}

#' Split a matrix into blocks, keeping declared `same()` blocks whole
#'
#' `lotriMatInv()` splits on connectivity, so a declared block with a
#' covariance of exactly zero comes back as two blocks.  That is fine for
#' the matrix itself, but a `same()` line has to be written against the
#' block as it was DECLARED or it re-parses against the wrong master.
#' The declared boundaries are recovered from the offsets: a copy run of
#' `k` rows means both it and its master are `k` rows wide.
#'
#' @param mat matrix to split
#' @return list of blocks, in order
#' @noRd
#' @author Matthew L. Fidler
.lotriSameSplit <- function(mat) {
  .f <- .lotriSameFamilies(mat, attr(mat, "lotriSame"))
  if (is.null(.f)) {
    ## nothing survived, so the emitters must not see the raw offsets
    ## either -- they would otherwise re-invent a linkage this view has
    ## just rejected
    attr(mat, "lotriSame") <- NULL
    return(lotriMatInv(mat)) # nolint
  }
  ## the blocks carry the VALIDATED offsets, so `.lotriSameEmit()`,
  ## `as.data.frame()` and `print()` cannot disagree
  attr(mat, "lotriSame") <- .f$same
  .n <- dim(mat)[1]
  .end <- logical(.n)
  .p <- 0L
  for (.b in lotriMatInv(mat)) { # nolint
    .p <- .p + dim(.b)[1]
    .end[.p] <- TRUE
  }
  for (.fm in .f$families) {
    for (.r in list(.fm$master, .fm$copy)) {
      .end[.r] <- FALSE
      .end[.r[length(.r)]] <- TRUE
    }
  }
  .end[.n] <- TRUE
  .ret <- list()
  .start <- 1L
  for (.i in seq_len(.n)) {
    if (!.end[.i]) next
    .ret[[length(.ret) + 1L]] <- .lotriSliceBlock(mat, .start:.i)
    .start <- .i + 1L
  }
  .ret
}

#' Work with `same()` (NONMEM `BLOCK SAME`) blocks in a lotri data frame
#'
#' A repeated block created with `same()` records, in the `condition`
#' column of `as.data.frame(<lotri>)`, which element of the block it
#' mirrors:
#'
#' \preformatted{
#' <baseCondition>:same:<masterEta>                 # diagonal row
#' <baseCondition>:same:<masterEta1>:<masterEta2>   # covariance row
#' }
#'
#' The master is named rather than indexed because `neta1`/`neta2` are
#' renumbered whenever parameters are added, dropped or reordered.
#'
#' These helpers are the supported way to consume that column.  Code that
#' compares `condition` directly (`condition == "id"`) will misclassify a
#' repeated block, so use `lotriBaseCondition()` for those tests.
#'
#' @param condition character vector of `condition` values, as found in
#'   the data frame produced by `as.data.frame()` on a `lotri` object.
#'
#' @param iniDf a lotri/rxode2 style data frame, with at least the
#'   `name`, `neta1`, `neta2` and `condition` columns.
#'
#' @param etas character vector of parameter names whose block has been
#'   structurally changed.
#'
#' @return
#'
#' - `lotriBaseCondition()`: the condition with any `:same:` suffix
#'   removed, the same length as `condition`.
#'
#' - `lotriIsSame()`: logical, `TRUE` where the row mirrors another.
#'
#' - `lotriSameMap()`: an integer vector over the eta indices of `iniDf`,
#'   `0` for an ordinary or master eta and otherwise the eta index of the
#'   master it mirrors.
#'
#' - `lotriSameBreak()`: `iniDf` with the `:same:` markers removed from
#'   every block that contains any of `etas`, so the copies become
#'   ordinary independent blocks.
#'
#' @examples
#'
#' mat <- lotri({
#'   iov.cl1 + iov.v1 ~ c(0.1,
#'                        0.01, 0.2)
#'   iov.cl2 + iov.v2 ~ same()
#' })
#'
#' df <- as.data.frame(mat)
#' df$condition
#'
#' lotriBaseCondition(df$condition)
#' lotriIsSame(df$condition)
#' lotriSameMap(df)
#'
#' @author Matthew L. Fidler
#' @export
lotriBaseCondition <- function(condition) {
  if (length(condition) == 0L) return(character(0))
  sub(":same:.*$", "", as.character(condition))
}

#' @rdname lotriBaseCondition
#' @export
lotriIsSame <- function(condition) {
  if (length(condition) == 0L) return(logical(0))
  .c <- as.character(condition)
  .r <- !is.na(.c) & .c != lotriBaseCondition(.c)
  .r
}

#' @rdname lotriBaseCondition
#' @export
lotriSameMap <- function(iniDf) {
  if (!inherits(iniDf, "data.frame")) {
    stop("'iniDf' must be a data.frame", call.=FALSE)
  }
  if (!all(c("name", "neta1", "neta2", "condition") %in% names(iniDf))) {
    stop("'iniDf' needs the 'name', 'neta1', 'neta2' and 'condition' columns",
         call.=FALSE)
  }
  .w <- which(!is.na(iniDf$neta1) & iniDf$neta1 == iniDf$neta2)
  if (length(.w) == 0L) return(integer(0))
  .idx <- iniDf$neta1[.w]
  .nme <- as.character(iniDf$name)[.w]
  .ret <- integer(max(.idx))
  .cnd <- as.character(iniDf$condition)[.w]
  .isSame <- lotriIsSame(.cnd)
  for (.i in which(.isSame)) {
    ## a diagonal row names exactly one master
    .m <- sub("^.*?:same:", "", .cnd[.i])
    .mw <- which(.nme == .m)
    if (length(.mw) != 1L) {
      stop("the 'same()' condition '", .cnd[.i], "' refers to '", .m,
           "', which is ",
           ifelse(length(.mw) == 0L, "not a parameter", "ambiguous"),
           " in this data frame", call.=FALSE)
    }
    .ret[.idx[.i]] <- as.integer(.idx[.mw])
  }
  .ret
}

#' @rdname lotriBaseCondition
#' @export
lotriSameBreak <- function(iniDf, etas) {
  if (!inherits(iniDf, "data.frame")) {
    stop("'iniDf' must be a data.frame", call.=FALSE)
  }
  if (length(etas) == 0L) return(iniDf)
  .cnd <- as.character(iniDf$condition)
  .isSame <- lotriIsSame(.cnd)
  if (!any(.isSame)) return(iniDf)
  .nme <- as.character(iniDf$name)
  .base <- lotriBaseCondition(.cnd)
  .masters <- lapply(seq_along(.cnd), function(.i) {
    if (!.isSame[.i]) return(character(0))
    strsplit(sub("^.*?:same:", "", .cnd[.i]), ":", fixed=TRUE)[[1]]
  })
  ## A "same family" is one master block together with every block that
  ## repeats it.  Group by union-find over the (copy, master) pairs the
  ## DIAGONAL rows give, so two unrelated families under one condition
  ## stay separate and an ordinary eta sharing the condition is not
  ## dragged in.
  .parent <- new.env(parent=emptyenv())
  .find <- function(a) {
    while (!is.null(.parent[[a]]) && .parent[[a]] != a) a <- .parent[[a]]
    a
  }
  .union <- function(a, b) {
    if (is.null(.parent[[a]])) .parent[[a]] <- a
    if (is.null(.parent[[b]])) .parent[[b]] <- b
    .ra <- .find(a)
    .rb <- .find(b)
    if (.ra != .rb) .parent[[.rb]] <- .ra
  }
  .isDiag <- !is.na(iniDf$neta1) & !is.na(iniDf$neta2) &
    iniDf$neta1 == iniDf$neta2
  for (.i in which(.isSame & .isDiag)) {
    .union(paste0(.base[.i], "\r", .masters[[.i]][1]),
           paste0(.base[.i], "\r", .nme[.i]))
  }
  ## An off diagonal row names both members of a block, which is what
  ## ties `iov.cl1` and `iov.v1` into ONE family; without it the two
  ## columns of a 2x2 repeated block look like two unrelated families
  ## and only half the block gets unlinked.
  .isOff <- !is.na(iniDf$neta1) & !is.na(iniDf$neta2) &
    iniDf$neta1 != iniDf$neta2
  for (.i in which(.isOff)) {
    .p <- .lotriCovPriorKeyNames(.nme[.i])
    if (length(.p) == 2L) {
      .union(paste0(.base[.i], "\r", .p[1]), paste0(.base[.i], "\r", .p[2]))
    }
  }
  ## which families are touched?
  .hitRoot <- character(0)
  for (.k in ls(.parent)) {
    if (sub("^.*?\r", "", .k) %in% etas) .hitRoot <- c(.hitRoot, .find(.k))
  }
  if (length(.hitRoot) == 0L) return(iniDf)
  ## clear the marker on every repeated row whose family was touched;
  ## an off diagonal row is placed by the master names it points at
  .hit <- vapply(seq_along(.cnd), function(.i) {
    if (!.isSame[.i]) return(FALSE)
    .k <- paste0(.base[.i], "\r", .masters[[.i]][1])
    if (is.null(.parent[[.k]])) return(FALSE)
    .find(.k) %in% .hitRoot
  }, logical(1), USE.NAMES=FALSE)
  iniDf$condition[which(.hit)] <- .base[which(.hit)]
  iniDf
}
