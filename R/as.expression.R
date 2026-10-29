#' Turn a character expression into quoted symbol
#'
#' @param chr Character symbol
#'
#' @return Quoted symbol
#'
#' @author Matthew Fidler
#'
#' @noRd
.enQuote <- function(chr) {
  eval(parse(text=paste0("quote(", chr, ")")))
}

#' Turn a single lotri estimate data frame estimate into lhs expression
#'
#' @param df1 Single Left hand side data row
#'
#' @return normalized left handed expression
#'
#' @author Matthew L. Fidler
#'
#' @noRd
.lotriLhsExprFromDf1 <- function(df1) {
  .ret <- list(ifelse(df1$fix, quote(`fix`), quote(`c`)),
               df1$lower, df1$est, df1$upper)
  if (.ret[[4]] == Inf) {
    .ret <- .ret[-4]
    if (.ret[[2]] == -Inf) {
      .ret <- .ret[-2]
      if (!df1$fix) return(.ret[[2]])
    }
  }
  eval(parse(text=paste0("quote(", .deparse1(as.call(.ret)), ")"))) # nolint
}
#' This returns the current initial estimate assigment based on df1
#'
#' @param df1 Single row of a parameter estimation statement
#' @return Quoted assignment expression
#' @author Matthew L. Fidler
#' @noRd
.lotriAssignmentExprFromDf1 <- function(df1) {
  call("<-", .enQuote(df1$name), .lotriLhsExprFromDf1(df1))
}
#' Returns the quoted `backTransform` argument
#'
#' @param df1 Single row of a parameter estimation statement
#' @return Quoted assignment expression
#' @author Matthew L. Fidler
#' @noRd
.lotriBackTransformFromDf1 <- function(df1) {
  if (is.na(df1$backTransform)) return(NULL)
  list(eval(parse(text=paste0("quote(backTransform(",
                              .deparse1(df1$backTransform), # nolint
                              "))"))))
}

#' Returns the quoted `label` argument
#'
#' @param df1 Single row of a parameter estimation statement
#' @return Quoted assignment expression
#' @author Matthew L. Fidler
#' @noRd
.lotriLabelFromDf1 <- function(df1) {
  if (is.na(df1$label)) return(NULL)
  list(eval(parse(text=paste0("quote(label(",
                              .deparse1(df1$label), # nolint
                              "))"))))
}
#'  This produces a list of quoted lines baesd on df1
#'
#' @param df1 Single row of an estimate data.frame
#' @return List of expression(s) equivalent to this line
#' @author Matthew L. Fidler
#' @noRd
.lotriExpressionLinesFromDf1 <- function(df1) {
  c(list(.lotriAssignmentExprFromDf1(df1)),
    .lotriBackTransformFromDf1(df1),
    .lotriLabelFromDf1(df1))
}
#'  This gets the "population" type of estimates per line
#'
#' @param df Data frame of estimates
#' @param lines Lines to consider when creating list
#' @return List of expressions to give the code for the data.frame
#' @author Matthew L. Fidler
#' @noRd
.lotriGetPopLinesFromDf <- function(df, lines) {
  if (missing(lines)) lines <- seq_along(df$name)
  do.call("c", lapply(lines, function(i) {
    df1 <- df[i, ]
    if (any(names(df1) == "ntheta")) {
      if (is.na(df1$ntheta)) {
        return(NULL)
      }
    }
    .lotriExpressionLinesFromDf1(df1)
  }))
}
#' Get ETA Matrix Elements in Line Form
#'
#' This function processes a matrix or a list of matrices to extract
#' ETA matrix elements and format them in a line form, that is:
#'
#' a ~ 1
#' b ~ c(1, 2)
#'
#' Which is different from the plus form
#'
#' a + b ~ c(1, 1, 2)
#'
#' @param x A matrix or a list of matrices. If a matrix, it is
#'   processed directly. If a list, each matrix in the list is
#'   processed.
#'
#' @param condition A character string specifying the condition to be
#'   applied. Default is `"id"`.
#'
#' @param nameEst An integer or logical value. If an integer, it
#'   specifies the maximum of dimension before the expression uses
#'   names. If logical, it indicates whether to use names for all expressions
#'
#' @return A list of language objects representing the formatted matrix elements.
#'
#' @details
#'
#' The function checks if the input `x` is a matrix or a list. If it
#' is a matrix, it changes the matrix to a lotri matrix list using
#' `lotriMatInv` and processes each element to format it according to
#' the specified condition and naming convention. If it is a list, the
#' function recursively processes each matrix in the list.
#'
#' @examples
#' # Example usage:
#'
#' mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
#' dimnames(mat) <- list(c("a", "b"), c("a", "b"))
#'
#' .lotriGetEtaLineForm(mat)
#'
#' @keywords internal
#' @author Matthew L. Fidler
#' @noRd
.lotriGetEtaLineForm <- function(x, condition="id", nameEst=5L) {
  if (inherits(x, "matrix")) {
    .x <- .lotriSameSplit(x)
    .sameEmit <- .lotriSameEmit(.x)
    .l <- lapply(seq_along(.x), function(i) {
      .mat <- .x[[i]]
      .labels <- attr(.mat, "lotriLabels")
      .lotriFix <- attr(.mat, "lotriFix")
      .fixOrC <- "c"
      if (!is.null(.lotriFix)) {
        if (all(.lotriFix)) {
          .fixOrC <- "fix"
        }
      }
      .nme <- dimnames(.mat)[[1]]
      if (is.logical(nameEst)) {
        .useNames <- nameEst
      } else {
        .useNames <- nameEst <= length(.nme)
      }
      .n <- length(.nme)
      if (.sameEmit[i]) {
        ## one line for the whole repeated block; a `+`-joined left hand
        ## side is legal in line-form output too
        .sameLab <- NULL
        if (!is.null(.labels)) {
          .l <- .labels[length(.labels)]
          if (!is.na(.l)) {
            .sameLab <- str2lang(paste0("quote(label(", deparse1(.l), "))"))
          }
        }
        return(list(list(
          str2lang(paste0("quote(", paste(.nme, collapse=" + "), " ~ same()",
                          ifelse(condition == "id", "",
                                 paste0("| ", condition)), ")")),
          .sameLab)))
      }
      lapply(seq_len(.n), function(i) {
        .c <- .fixOrC
        if (!is.null(.lotriFix)) {
          if (all(.lotriFix[seq(1, i), i])) {
            .c <- "fix"
          } else {
            .c <- "c"
          }
        }
        .vals <- vapply(seq_len(i), function(j) {
          .fix <- FALSE
          if (.c != "fix" && !is.null(.lotriFix)) {
            .fix <- .lotriFix[i, j]
          }
          if (.fix) {
            if (.useNames) {
              paste0(.nme[j], "= fix(", .mat[i, j], ")")
            }  else {
              paste0("fix(", .mat[i, j], ")")
            }
          } else {
            if (.useNames) {
              paste0(.nme[j], "=", .mat[i, j])
            }  else {
              paste0(.mat[i, j])
            }
          }
        }, character((1)), USE.NAMES=FALSE)
        if (is.null(.labels)) {
          .lab <- NULL
        } else {
          .lab <- .labels[i]
          if (!is.na(.lab)) {
            .lab <- str2lang(paste0("quote(label(", deparse1(.lab), "))"))
          } else {
            .lab <- NULL
          }
        }
        if (length(.vals) == 1 && .c == "c" && !.useNames) {
          list(str2lang(paste0("quote(",
                               .nme[i], "~ ", .vals,
                               ifelse(condition == "id", "", paste0("| ", condition)), ")")),
               .lab)
        } else {
          list(str2lang(paste0("quote(", .nme[i], "~ ", .c,
                               "(",paste(.vals, collapse=", "), ")",
                               ifelse(condition == "id", "", paste0("| ", condition)), ")")),
               .lab)
        }
      })
    })
    ## a matrix with no etas at all (ie an estimate only lotri) gives an
    ## empty list here, and `do.call(c, NULL)` is an error
    .u <- unlist(.l)
    if (is.null(.u)) return(NULL)
    do.call(`c`, .u)
  } else if (inherits(x, "list")) {
    .n <- names(x)
    do.call("c", lapply(.n, function(nme) {
      .lotriGetEtaLineForm(x[[nme]],
                           condition=nme,
                           nameEst=nameEst)
    }))
  }
}

#' Which blocks may be re-emitted as `~ same()`?
#'
#' A `lotriSame` offset is relative and is only re-parseable as `same()`
#' when it lands exactly on the start of an earlier block of the same
#' dimension.  A matrix rebuilt from a hand-edited data frame can carry
#' an offset that does not, so this is checked rather than assumed; a
#' block that fails falls back to being written out with its explicit
#' values, which is still a valid matrix, just without the annotation.
#'
#' @param x list of blocks, as returned by `lotriMatInv()`
#' @return logical vector, one per block
#' @noRd
#' @author Matthew L. Fidler
.lotriSameEmit <- function(x) {
  .starts <- integer(length(x))
  .pos <- 0L
  for (.i in seq_along(x)) {
    .starts[.i] <- .pos
    .pos <- .pos + dim(x[[.i]])[1]
  }
  ## filled in order: whether a block may be written as `same()` depends
  ## on whether the blocks BEFORE it were, so this cannot be a `vapply`
  ## over independent elements
  .ok <- logical(length(x))
  for (.i in seq_along(x)) {
    .ok[.i] <- FALSE
    .s <- attr(x[[.i]], "lotriSame")
    if (is.null(.s)) next
    if (any(.s == 0L)) next
    if (length(unique(.s)) != 1L) next
    .w <- which(.starts == .starts[.i] - .s[1])
    if (length(.w) != 1L) next
    if (!isTRUE(dim(x[[.w]])[1] == dim(x[[.i]])[1])) next
    ## the master must not itself be a copy: re-parsing `same()` always
    ## repeats the ORIGINAL block, so emitting it for a block that
    ## mirrors a mirror would come back with different offsets
    .ms <- attr(x[[.w]], "lotriSame")
    if (!is.null(.ms) && any(.ms != 0L)) next
    ## A re-parsed `same()` repeats the IMMEDIATELY PRECEDING block, so
    ## every block between the master and this one must itself be
    ## WRITTEN as `same()` against that master.  Checking only that they
    ## carry the right offsets is not enough: a block that carries them
    ## but fails its own guard (a label it cannot express, say) is
    ## written out with explicit values, and this `same()` would then
    ## repeat THAT block instead of the master.
    if (.i > .w + 1L) {
      .between <- seq(.w + 1L, .i - 1L)
      if (!all(.ok[.between])) next
      ## nocov start
      ## Belt and braces.  A block that is `.ok` was itself accepted
      ## above, and 283 rejects a copy of a copy, so its master is the
      ## same original this one points at -- there is no input reaching
      ## here that fails this.  Kept because the reasoning is subtle and
      ## the cost of being wrong is a matrix that re-parses differently.
      if (!all(vapply(.between, function(.b) {
        .bs <- attr(x[[.b]], "lotriSame")
        !is.null(.bs) && .starts[.b] - .bs[1] == .starts[.w]
      }, logical(1)))) next
      ## nocov end
    }
    ## exact, not `all.equal()`'s default tolerance: a genuine copy is
    ## bit identical to its master, and collapsing blocks that merely
    ## agree to ~1e-8 would change the values on the round trip
    if (!isTRUE(all.equal(unclass(x[[.i]]), unclass(x[[.w]]),
                          check.attributes=FALSE, tolerance=0))) {
      next
    }
    .fi <- attr(x[[.i]], "lotriFix")
    .fw <- attr(x[[.w]], "lotriFix")
    if (is.null(.fi) != is.null(.fw)) next
    if (!is.null(.fi) && !identical(unname(.fi), unname(.fw))) next
    ## a `same()` line can carry only ONE trailing `label()`, which
    ## attaches to the last name; a label anywhere else would be dropped
    .li <- attr(x[[.i]], "lotriLabels")
    if (!is.null(.li) && length(.li) > 1L &&
          any(!is.na(.li[-length(.li)]))) {
      next
    }
    .ok[.i] <- TRUE
  }
  .ok
}

#' Get the eta matrix elements for a lotri matrix
#'
#' @param x lotri matrix
#' @param condition Condition, if neeeded
#' @return list expression
#' @author Matthew L. Fidler
#' @noRd
.lotriGetEtaMatEltPlusForm <- function(x, condition="id") {
  if (inherits(x, "matrix")) {
    .x <- .lotriSameSplit(x)
    .sameEmit <- .lotriSameEmit(.x)
    .l <- lapply(seq_along(.x), function(i) {
      .mat <- .x[[i]]
      .nme <- dimnames(.mat)[[1]]
      if (.sameEmit[i]) {
        return(eval(expr=parse(text=paste0(
          "quote(", paste(.nme, collapse="+"), "~ same()",
          ifelse(condition == "id", "", paste0("| ", condition)), ")"))))
      }
      .n <- length(.nme)
      .v <- vector("numeric", .n * (.n + 1) / 2)
      .k <- 1
      for (.i in seq(1, .n)) {
        for (.j in seq(1, .i)) {
          .v[.k] <- .mat[.i, .j]
          .k <- .k + 1
        }
      }
      .v0 <- .deparse1(.v) # nolint
      .lotriFix <- attr(.mat, "lotriFix")
      if (!is.null(.lotriFix)) {
        if (all(.lotriFix)) {
          if (length(.v) > 1) {
            .v0 <- paste0("fix", substr(.v0, 2, nchar(.v0)))
          } else {
            .v0 <- paste0("fix(", .v0, ")")
          }
        }
      }
      eval(expr=parse(text=paste0("quote(", paste(.nme, collapse="+"), "~", .v0,
                                  ifelse(condition == "id", "", paste0("| ", condition)), ")")))
    })
    .l
  } else if (inherits(x, "list")) {
    .n <- names(x)
    do.call("c", lapply(.n, function(nme) {
      .lotriGetEtaMatEltPlusForm(x[[nme]], condition=nme)
    }))
  }
}

#' Convert a lotri data frame to a lotri expression
#'
#' @param data lotri data frame
#'
#' @param useIni Use `ini` instead of `lotri` in the expression
#'
#' @return expression of the lotri syntax equivalent to the data.frame provided
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#'  x <- lotri({
#'   tka <- 0.45; label("Log Ka")
#'   tcl <- 1; label("Log Cl")
#'   tv <- 3.45; label("Log V")
#'   eta.ka ~ 0.6
#'   eta.cl ~ 0.3
#'   eta.v ~ 0.1
#'   add.err <- 0.7
#' })
#'
#' df <- as.data.frame(x)
#'
#' lotriDataFrameToLotriExpression(df)
#'
#' # You may also call as.expression directly from the lotri object
#'
#' as.expression(x)
#'
#' @export
lotriDataFrameToLotriExpression <- function(data, useIni=FALSE) { # nolint
  if (!inherits(data, "data.frame")) stop("input must be lotri data.frame", call.=FALSE)
  .l <- as.lotri(data) # nolint
  as.expression(.l, useIni=useIni)
}

#' Build the `prior(name) ~ dist(...)` lines
#'
#' Priors are emitted as a trailing group, which is safe because the
#' syntax names its target and is therefore order independent.
#'
#' @param est `lotriEst` data frame (may be NULL)
#' @param mat matrix or list of matrices (may be NULL)
#' @return list of quoted prior lines
#' @noRd
#' @author Matthew L. Fidler
.lotriGetPriorLines <- function(est, mat) {
  .ret <- list()
  .add <- function(nms, txt) {
    .e <- try(str2lang(paste0("prior(", paste(nms, collapse=", "), ") ~ ", txt)),
              silent=TRUE)
    if (inherits(.e, "try-error")) {
      ## a prior is validated when it is parsed, so this only happens if
      ## the column was written to by hand; say so rather than making the
      ## object impossible to print
      warning("cannot deparse the prior on '", paste(nms, collapse=", "),
              "': ", txt, call.=FALSE)
      return(invisible())
    }
    .ret[[length(.ret) + 1L]] <<- .e
  }
  .isMultiPrior <- function(txt) {
    .fn <- try(str2lang(txt)[[1]], silent=TRUE)
    if (inherits(.fn, "try-error")) return(FALSE)
    .dist <- .lotriPriorLookup(as.character(.fn))
    !is.null(.dist) && .dist$kind %in% c("matrix", "multivariate")
  }
  ## A joint theta + `om.` block is stored once, on the first name of the
  ## block, and the block spans both the estimates and the omega -- so
  ## neither of the loops below can recover its members.  Its covariance
  ## names every one of them, and an `om.` name marks it as joint.
  .jointNames <- function(txt) {
    .nms <- .lotriPriorCovNames(txt)
    if (is.null(.nms) || !any(grepl("^om[.].", .nms))) return(NULL)
    .nms
  }
  if (!is.null(est) && any(names(est) == "prior")) {
    .done <- rep(FALSE, length(est$name))
    for (.i in seq_along(est$name)) {
      if (.done[.i] || is.na(est$prior[.i])) next
      .txt <- est$prior[.i]
      .jnt <- .jointNames(.txt)
      if (!is.null(.jnt)) {
        .done[.i] <- TRUE
        .add(.jnt, .txt)
        next
      }
      if (.isMultiPrior(.txt)) {
        ## a multivariate prior is stored on every estimate it covers, so
        ## the group is recovered by the estimates that share it
        .w <- which(!is.na(est$prior) & est$prior == .txt)
      } else {
        ## two estimates can legitimately have the same univariate prior,
        ## so those must stay separate lines
        .w <- .i
      }
      .done[.w] <- TRUE
      .add(est$name[.w], .txt)
    }
  }
  .mats <- NULL
  if (is.matrix(mat)) {
    .mats <- list(mat)
  } else if (inherits(mat, "list") || inherits(mat, "lotri")) {
    .mats <- as.list(mat)
  }
  for (.m in .mats) {
    if (!is.matrix(.m)) next
    .p <- attr(.m, "lotriPriors")
    .dn <- dimnames(.m)[[1]]
    if (!is.null(.p)) {
      for (.i in seq_along(.p)) {
        if (is.na(.p[.i])) next
        .nms <- .dn[.i]
        .jnt <- .jointNames(.p[.i])
        if (!is.null(.jnt)) {
          .add(.jnt, .p[.i])
          next
        }
        .fn <- try(str2lang(.p[.i])[[1]], silent=TRUE)
        if (!inherits(.fn, "try-error")) {
          .dist <- .lotriPriorLookup(as.character(.fn))
          if (!is.null(.dist) && .dist$kind %in% c("matrix", "multivariate")) {
            ## a block prior is stored on the first diagonal of the block,
            ## so recover the rest of the block for the round trip
            .nms <- .dn[.lotriBlockIndexes(.m, .i)]
          }
        }
        .add(.nms, .p[.i])
      }
    }
    .pOff <- attr(.m, "lotriOffDiagPriors")
    if (!is.null(.pOff)) {
      for (.key in names(.pOff)) {
        .add(.lotriCovPriorKeyNames(.key), .pOff[[.key]])
      }
    }
  }
  .ret
}

#' Rebuild the `dist(eta) ~ family(...)` lines of an object
#'
#' Like the prior lines, these are emitted after the matrix so the
#' random effect they name has already been declared.
#'
#' @param mat matrix or list of matrices (may be NULL)
#' @return list of quoted eta distribution lines
#' @noRd
#' @author Matthew L. Fidler
.lotriGetEtaDistLines <- function(mat) {
  .ret <- list()
  .mats <- NULL
  if (is.matrix(mat)) {
    .mats <- list(mat)
  } else if (inherits(mat, "list") || inherits(mat, "lotri")) {
    .mats <- as.list(mat)
  }
  for (.m in .mats) {
    if (!is.matrix(.m)) next
    .d <- attr(.m, "lotriEtaDists")
    if (is.null(.d)) next
    .dn <- dimnames(.m)[[1]]
    for (.i in seq_along(.d)) {
      if (is.na(.d[.i])) next
      .e <- try(str2lang(paste0("dist(", .dn[.i], ") ~ ", .d[.i])), silent=TRUE)
      if (inherits(.e, "try-error")) {
        warning("cannot deparse the distribution declared on '", .dn[.i],
                "': ", .d[.i], call.=FALSE)
        next
      }
      .ret[[length(.ret) + 1L]] <- .e
    }
  }
  .ret
}

#' @export
as.expression.lotriFix <- function(x, ...) {
  .lst <- list(...)
  if (!any(names(.lst) == "useIni")) {
    .lst$useIni <- FALSE
  }
  if (!any(names(.lst) == "plusNames")) {
    .lst$plusNames <- getOption("lotri.plusNames", FALSE)
  }
  if (!any(names(.lst) == "nameEst")) {
    .lst$nameEst <- getOption("lotri.nameEst", 5L)
  }
  .l <- x
  .est <- attr(.l, "lotriEst")
  .mat <- .l
  attr(.mat, "lotriEst") <- NULL
  class(.mat) <- NULL
  .priorLines <- c(.lotriGetPriorLines(.est, .mat),
                   .lotriGetEtaDistLines(.mat))
  if (!.lst$plusNames) {
    as.call(list(ifelse(.lst$useIni, quote(`ini`), quote(`lotri`)),
                 as.call(c(list(quote(`{`)), .lotriGetPopLinesFromDf(.est),
                           .lotriGetEtaLineForm(.mat, nameEst=.lst$nameEst),
                           .priorLines))))
  } else {
    as.call(list(ifelse(.lst$useIni, quote(`ini`), quote(`lotri`)),
                 as.call(c(list(quote(`{`)), .lotriGetPopLinesFromDf(.est),
                           .lotriGetEtaMatEltPlusForm(.mat),
                           .priorLines))))
  }
}

#' Change a matrix or lotri matrix to a lotri expression
#'
#' @param x matrix
#'
#' @param useIni use the ini block
#'
#' @param plusNames logical, when `TRUE` use the `a + b ~ c(1, 0.1,
#'   1)` naming convention.  Otherwise use the lotri single line
#'   convention `a ~ 1; b ~ c(0.1, 1)`
#'
#' @param nameEst logical or integerish.  When logical `TRUE` will add
#'   names to all matrix estimates and `TRUE` when using the lotri
#'   single line convention i.e. `a~c(a=1); b~c(a=0.1, b=1)`.  When an
#'   integer, the dimension of the matrix being displayed needs to
#'   have a dimension above this number before names are displayed.
#'
#' @export
lotriAsExpression <- function(x, useIni=FALSE,
                              plusNames=getOption("lotri.plusNames", FALSE),
                              nameEst=getOption("lotri.nameEst", 5L)) {
  checkmate::assertLogical(useIni, any.missing=FALSE, len=1)
  checkmate::assertLogical(plusNames, any.missing=FALSE, len=1)
  if (is.logical(nameEst)) {
    checkmate::assertLogical(nameEst, any.missing=FALSE, len=1)
  } else  {
    checkmate::assertIntegerish(nameEst, any.missing=FALSE, len=1, lower=1)
  }
  as.expression.lotriFix(x, useIni=useIni, plusNames=plusNames, nameEst=nameEst)
}
