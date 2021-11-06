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
