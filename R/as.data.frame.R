.as.data.frame.lotriFix.mat <- function(mat, condition="ID") {
  .df3 <- NULL
  if (inherits(mat, "matrix")) {
    .lst2 <- lotriMatInv(mat)
    .eta1 <- 1
    .eta2 <- 1
    for (.i in seq_along(.lst2)) {
      .curMat <- .lst2[[.i]]
      .curMatF <- attr(.curMat, "lotriFix")
      .n <- dimnames(.curMat)[[1]]
      for (.j in seq_along(.n)) {
        for (.k in seq_len(.j)) {
          if (.j == .k) {
            .curName <- .n[.j]
          } else {
            .curName <- paste0("(", .n[.k], ",", .n[.j], ")")
          }
          .fix <- FALSE
          if (!is.null(.curMatF)) {
            .fix <- .curMatF[.j, .k]
          }
          .df3 <- rbind(.df3,
                        data.frame(ntheta=NA_integer_,
                                   neta1=.j, neta2=.k,
                                   name=.curName,
                                   lower= -Inf,
                                   est=.curMat[.j, .k],
                                   upper=Inf,
                                   fix=.fix,
                                   label=NA_integer_,
                                   backTransform=NA_character_,
                                   condition=condition))
        }
      }
    }
  }
}

##'@export
as.data.frame.lotriFix <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (!missing(row.names)) {
    stop("'row.names' should not be used when converting lotri object to data.frame",
         call.=FALSE)
  }
  if (!missing(optional)) {
    stop("'optional' should not be used when converting lotri object to data.frame",
         call.=FALSE)
  }
  .df <- lotriEst(x, drop=FALSE)
  if (!is.null(.df)) {
    .df$ntheta <- seq_along(.df$est)
    .df$neta1 <- NA_integer_
    .df$neta2 <- NA_integer_
    .df$condition <- NA_character_
  }
  .df2 <- lotriEst(x, drop=TRUE)
  .df3 <- NULL
  if (inherits(.df2, "matrix")) {
    .df3 <- .as.data.frame.lotriFix.mat(.df2)
  }
  if (!is.null(.df2)) {
    .lst2 <- lotriMatInv(.df2)
    .eta1 <- 1
    .eta2 <- 1
    for (.i in seq_along(.lst2)) {
      .curMat <- .lst2[[.i]]
      .curMatF <- attr(.curMat, "lotriFix")
      .n <- dimnames(.curMat)[[1]]
      for (.j in seq_along(.n)) {
        for (.k in seq_len(.j)) {
          if (.j == .k) {
            .curName <- .n[.j]
          } else {
            .curName <- paste0("(", .n[.k], ",", .n[.j], ")")
          }
          .fix <- FALSE
          if (!is.null(.curMatF)) {
            .fix <- .curMatF[.j, .k]
          }
          .df3 <- rbind(.df3,
                        data.frame(ntheta=NA_integer_,
                                   neta1=.j, neta2=.k,
                                   name=.curName,
                                   lower= -Inf,
                                   est=.curMat[.j, .k],
                                   upper=Inf,
                                   fix=.fix,
                                   label=NA_integer_,
                                   backTransform=NA_character_,
                                   condition="ID"))
        }
      }
    }
  }


  .ord <- c("ntheta", "neta1", "neta2", "name", "lower", "est", "upper", "fix", "label", "backTransform", "condition")
  .df <- rbind(.df, .df3)
  return(.df[, .ord])

  ##   ntheta neta1 neta2   name lower       est   upper   fix  err  label
  ## 1      1    NA    NA    tka  -Inf 0.4500000     Inf FALSE <NA> Log Ka
  ## 2      2    NA    NA    tcl  -Inf 0.9932518 4.60517 FALSE <NA> Log Cl
  ## 3      3    NA    NA     tv  -Inf 3.4500000     Inf FALSE <NA>  log V
  ## 4     NA     1     1 eta.ka  -Inf 0.6000000     Inf FALSE <NA>   <NA>
  ## 5     NA     2     2 eta.cl  -Inf 0.3000000     Inf FALSE <NA>   <NA>
  ## 6     NA     3     3  eta.v  -Inf 0.1000000     Inf FALSE <NA>   <NA>
  ## 7      4    NA    NA add.sd     0 0.7000000     Inf FALSE  add   <NA>
  ##   backTransform condition trLow trHi
  ## 1                    <NA>  -Inf  Inf
  ## 2                    <NA>  -Inf  Inf
  ## 3                    <NA>  -Inf  Inf
  ## 4                      ID  -Inf  Inf
  ## 5                      ID  -Inf  Inf
  ## 6                      ID  -Inf  Inf
  ## 7                linCmt()  -Inf  Inf
}
