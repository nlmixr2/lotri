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
  eval(parse(text=paste0("quote(", deparse1(as.call(.ret)), ")")))
}
#' This returns the current initial estimate assigment based on df1
#'
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
    if (is.na(df1$ntheta)) return(NULL)
    .lotriExpressionLinesFromDf1(df1)
  }))
}


