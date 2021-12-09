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

#' Get the eta matrix elements for a lotri matrix
#'
#' @param x lotri matrix
#' @param condition Condition, if neeeded
#' @return list expression
#' @author Matthew L. Fidler
#' @noRd
.lotriGetEtaMatrixElements <- function(x, condition="id") {
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
      .lotriGetEtaMatrixElements(x[[nme]], condition=nme)
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
  .l <- x
  .est <- attr(.l, "lotriEst")
  .mat <- .l
  attr(.mat, "lotriEst") <- NULL
  class(.mat) <- NULL
  as.call(list(ifelse(.lst$useIni, quote(`ini`), quote(`lotri`)),
               as.call(c(list(quote(`{`)), .lotriGetPopLinesFromDf(.est),
                          .lotriGetEtaMatrixElements(.mat)))))
}
