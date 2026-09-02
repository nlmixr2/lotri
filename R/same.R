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
