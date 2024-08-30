#' Turn a character expression into quoted symbol
#'
#' @param chr Character symbol
#' @return Quoted symbol
#' @author Matthew Fidler
#' @noRd
.enQuote <- function(chr) {
  eval(parse(text=paste0("quote(", chr, ")")))
}

#' Turn a single lotri estimate data frame estimate into lhs expression
#'
#' @param df1 Single Left hand side data row
#' @return normalized left handed expression
#' @author Matthew L. Fidler
#' @noRd
.lotriLhsExpressionFromDf1 <- function(df1) {
  .ret <- list(ifelse(df1$fix, quote(`fix`), quote(`c`)),
               df1$lower, df1$est, df1$upper)
  if (.ret[[4]] == Inf) {
    .ret <- .ret[-4]
    if (.ret[[2]] == -Inf) {
      .ret <- .ret[-2]
      if (!df1$fix) return(.ret[[2]])
    }
  }
  eval(parse(text=paste0("quote(", .deparse1(as.call(.ret)), ")")))
}
#' This returns the current initial estimate assigment based on df1
#'
#' @param df1 Single row of a parameter estimation statement
#' @return Quoted assignment expression
#' @author Matthew L. Fidler
#' @noRd
.lotriAssignmentExpressionFromDf1 <- function(df1) {
  call("<-", .enQuote(df1$name), .lotriLhsExpressionFromDf1(df1))
}
#' Returns the quoted `backTransform` argument
#'
#' @param df1 Single row of a parameter estimation statement
#' @return Quoted assignment expression
#' @author Matthew L. Fidler
#' @noRd
.lotriBackTransformFromDf1 <- function(df1) {
  if (is.na(df1$backTransform)) return(NULL)
  list(eval(parse(text=paste0("quote(backTransform(",.deparse1(df1$backTransform), "))"))))
}

#' Returns the quoted `label` argument
#'
#' @param df1 Single row of a parameter estimation statement
#' @return Quoted assignment expression
#' @author Matthew L. Fidler
#' @noRd
.lotriLabelFromDf1 <- function(df1) {
  if (is.na(df1$label)) return(NULL)
  list(eval(parse(text=paste0("quote(label(",.deparse1(df1$label), "))"))))
}
#'  This produces a list of quoted lines baesd on df1
#'
#' @param df1 Single row of an estimate data.frame
#' @return List of expression(s) equivalent to this line
#' @author Matthew L. Fidler
#' @noRd
.lotriExpressionLinesFromDf1 <- function(df1) {
  c(list(.lotriAssignmentExpressionFromDf1(df1)),
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
  do.call("c", lapply(lines, function(i){
    df1 <- df[i, ]
    if (any(names(df1) == "ntheta")){
      if (is.na(df1$ntheta)) return(NULL)
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
#' .lotriGetEtaMatrixElementsLineForm(mat)
#'
#' @keywords internal
#' @author Matthew L. Fidler
#' @noRd
.lotriGetEtaMatrixElementsLineForm <- function(x, condition="id", nameEst=5L) {
  if (inherits(x, "matrix")) {
    .x <- lotriMatInv(x)
    .l <- lapply(seq_along(.x), function(i) {
      .mat <- .x[[i]]
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
        if (length(.vals) == 1 && .c == "c" && !.useNames) {
          str2lang(paste0(.nme[i], "~ ", .vals,
                          ifelse(condition == "id", "", paste0("| ", condition))))
        } else {
          str2lang(paste0(.nme[i], "~ ", .c,
                          "(",paste(.vals, collapse=", "), ")",
                          ifelse(condition == "id", "", paste0("| ", condition))))
        }
      })
    })
    do.call(`c`, .l)
  } else if (inherits(x, "list")) {
    .n <- names(x)
    do.call("c", lapply(.n, function(nme) {
      .lotriGetEtaMatrixElementsLineForm(x[[nme]],
                                         condition=nme,
                                         nameEst=nameEst)
    }))
  }
}

#' Get the eta matrix elements for a lotri matrix
#'
#' @param x lotri matrix
#' @param condition Condition, if neeeded
#' @return list expression
#' @author Matthew L. Fidler
#' @noRd
.lotriGetEtaMatrixElementsPlusForm <- function(x, condition="id") {
  if (inherits(x, "matrix")) {
    .x <- lotriMatInv(x)
    .l <- lapply(seq_along(.x), function(i) {
      .mat <- .x[[i]]
      .nme <- dimnames(.mat)[[1]]
      .n <- length(.nme)
      .v <- vector("numeric", .n * (.n + 1) / 2)
      .k <- 1
      for (.i in seq(1, .n)) {
        for (.j in seq(1, .i)) {
          .v[.k] <- .mat[.i, .j]
          .k <- .k + 1
        }
      }
      .v0 <- .deparse1(.v)
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
    do.call("c", lapply(.n, function(nme){
      .lotriGetEtaMatrixElementsPlusForm(x[[nme]], condition=nme)
    }))
  }
 }

#' Convert a lotri data frame to a lotri expression
#'
#' @param data lotri data frame
#' @param useIni Use `ini` instead of `lotri` in the expression
#' @return expression of the lotri syntax equivalent to the data.frame provided
#' @author Matthew L. Fidler
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
lotriDataFrameToLotriExpression <- function(data, useIni=FALSE) {
  if (!inherits(data, "data.frame")) stop("input must be lotri data.frame", call.=FALSE)
  .l <- as.lotri(data)
  as.expression(.l, useIni=useIni)
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
  if (!.lst$plusNames) {
    as.call(list(ifelse(.lst$useIni, quote(`ini`), quote(`lotri`)),
                 as.call(c(list(quote(`{`)), .lotriGetPopLinesFromDf(.est),
                           .lotriGetEtaMatrixElementsLineForm(.mat, nameEst=.lst$nameEst)))))
  } else {
    as.call(list(ifelse(.lst$useIni, quote(`ini`), quote(`lotri`)),
                 as.call(c(list(quote(`{`)), .lotriGetPopLinesFromDf(.est),
                            .lotriGetEtaMatrixElementsPlusForm(.mat)))))
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
