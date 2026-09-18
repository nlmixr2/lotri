.as.data.frame.lotriFix.mat <- function(mat, default="id",
                                        eta1=1) {
  .df3 <- NULL
  .env <- new.env(parent=emptyenv())
  .env$eta1 <- eta1
  if (inherits(mat, "matrix")) {
    .priors <- attr(mat, "lotriPriors")
    .priorsOffDiag <- attr(mat, "lotriOffDiagPriors")
    .etaDists <- attr(mat, "lotriEtaDists")
    .matNames <- dimnames(mat)[[1]]
    ## validated once against the WHOLE condition matrix, then indexed by
    ## the row's position within it -- the per block attribute cannot be
    ## checked on its own, since a copy's offset points outside its block
    .cleanSame <- .lotriSameClean(mat, attr(mat, "lotriSame"))
    .lst2 <- lotriMatInv(mat) # nolint
    for (.i in seq_along(.lst2)) {
      .curMat <- .lst2[[.i]]
      .curMatF <- attr(.curMat, "lotriFix")
      .curMatU <- attr(.curMat, "lotriUnfix")
      ## read per block: the top level pass this replaces could only see
      ## the labels of a single matrix, so a multi-condition object lost
      ## every label it had
      .curLab <- attr(.curMat, "lotriLabels")
      .n <- dimnames(.curMat)[[1]]
      for (.j in seq_along(.n)) {
        for (.k in seq_len(.j)) {
          if (.j == .k) {
            .curName <- .n[.j]
          } else {
            .curName <- paste0("(", .n[.k], ",", .n[.j], ")")
          }
          if (!is.null(.curMatF)) {
            .fix <- .curMatF[.j, .k]
          } else if (!is.null(.curMatU)) {
            .fix <- !.curMatU[.j, .k]
          } else {
            .fix <- FALSE
          }
          .curPrior <- NA_character_
          ## a declared eta distribution only ever sits on a diagonal, so
          ## it needs no off diagonal key the way a covariance prior does
          .curEtaDist <- NA_character_
          if (.j == .k && !is.null(.etaDists)) {
            .wd <- match(.n[.j], .matNames)
            if (!is.na(.wd)) .curEtaDist <- .etaDists[.wd]
          }
          if (.j == .k) {
            if (!is.null(.priors)) {
              ## priors are matched by name so they survive `rcm`
              .wp <- match(.n[.j], .matNames)
              if (!is.na(.wp)) .curPrior <- .priors[.wp]
            }
          } else if (!is.null(.priorsOffDiag)) {
            ## `.curName` (built above) is the SAME "(name_k,name_j)" key a
            ## covariance-pair prior is stored under (.lotriResolvePriors());
            ## matched by name, like the diagonal case, so it survives `rcm`
            .wp <- match(.curName, names(.priorsOffDiag))
            if (!is.na(.wp)) .curPrior <- .priorsOffDiag[[.wp]]
          }
          ## a repeated (`same()`) block records, in the condition, the
          ## element of the block it mirrors -- by NAME, since the eta
          ## numbers are renumbered by consumers of this data frame.  The
          ## offsets are relative and point outside this block, so the
          ## master is resolved against the whole matrix's dimnames.
          .cnd <- default
          ## `.env$eta1` counts GLOBALLY across conditions, while
          ## `.cleanSame`/`.matNames` are indexed within this condition
          .lj <- .env$eta1 + .j - eta1
          .lk <- .env$eta1 + .k - eta1
          ## both ends of a cell must be mirrored by the SAME offset, or
          ## the cell does not repeat anything as a whole
          if (!is.null(.cleanSame) && .cleanSame[.lj] > 0L &&
                .cleanSame[.lj] == .cleanSame[.lk]) {
            .mj <- .lj - .cleanSame[.lj]
            .mk <- .lk - .cleanSame[.lk]
            ## smaller index first, matching the "(name_k,name_j)" order
            ## the `name` column uses for an off diagonal
            .cnd <- paste0(default, ":same:", .matNames[.mk])
            if (.j != .k) {
              .cnd <- paste0(.cnd, ":", .matNames[.mj])
            }
          }
          .df3 <- rbind(.df3,
                        data.frame(ntheta=NA_integer_,
                                   neta1=.env$eta1 + .j - 1,
                                   neta2=.env$eta1 + .k - 1,
                                   name=.curName,
                                   lower= -Inf,
                                   est=.curMat[.j, .k],
                                   upper=Inf,
                                   fix=.fix,
                                   label=if (.j == .k && !is.null(.curLab)) {
                                     as.character(.curLab[.j])
                                   } else {
                                     NA_character_
                                   },
                                   backTransform=NA_character_,
                                   prior=.curPrior,
                                   etaDist=.curEtaDist,
                                   condition=.cnd))
        }
      }
      .env$eta1 <- max(.df3$neta1) + 1
    }
  }
  .df3
}

##'@export
as.data.frame.lotriFix <- function(x, row.names = NULL, optional = FALSE, ...,
                                   default="id") {
  if (!missing(row.names)) {
    stop("'row.names' should not be used when converting lotri object to data.frame",
         call.=FALSE)
  }
  if (!missing(optional)) {
    stop("'optional' should not be used when converting lotri object to data.frame",
         call.=FALSE)
  }
  .df <- lotriEst(x, drop=FALSE) # nolint
  if (!is.null(.df)) {
    if (length(.df$est) > 0) {
      .df$ntheta <- seq_along(.df$est)
      .df$neta1 <- NA_integer_
      .df$neta2 <- NA_integer_
      .df$condition <- NA_character_
    }
  }
  .df2 <- lotriEst(x, drop=TRUE) # nolint
  .df3 <- NULL
  if (inherits(.df2, "matrix")) {
    .df3 <- .as.data.frame.lotriFix.mat(.df2, default=default)
  } else if (inherits(.df2, "list") || inherits(.df2, "lotri")) {
    .env <- new.env(parent=emptyenv())
    .env$eta1 <- 1
    .df3 <- do.call(rbind,
                    lapply(names(.df2), function(default) {
                      .ret <- .as.data.frame.lotriFix.mat(.df2[[default]], default=default,
                                                          eta1=.env$eta1)
                      assign("eta1", .env$eta1 + dim(.df2[[default]])[1],
                             envir=.env)
                      .ret
                    }))
  }
  .ord <- c("ntheta", "neta1", "neta2", "name", "lower", "est", "upper", "fix", "label", "backTransform", "condition", "prior")
  if (!is.null(.df) && !any(names(.df) == "prior")) {
    ## `rep()` so that a zero row estimate frame stays zero row
    .df$prior <- rep(NA_character_, nrow(.df))
  }
  ## the `etaDist` column is only carried when something declares one, so
  ## a model without a declared random effect distribution produces the
  ## byte identical frame it always did
  .hasEtaDist <- !is.null(.df3) && any(!is.na(.df3$etaDist))
  if (.hasEtaDist) {
    .ord <- c(.ord, "etaDist")
    if (!is.null(.df)) .df$etaDist <- rep(NA_character_, nrow(.df))
  } else if (!is.null(.df3)) {
    .df3$etaDist <- NULL
  }
  .df <- rbind(.df, .df3)
  if (length(.df) == 0) {
    return(data.frame(ntheta=integer(0),
                      neta1=numeric(0),
                      neta2=numeric(0),
                      name=character(0),
                      lower=numeric(0),
                      est=numeric(0),
                      upper=numeric(0),
                      fix=numeric(0),
                      label=character(0),
                      backTransform=character(0),
                      condition=character(0),
                      prior=character(0)))
  }
  .df[, .ord]

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
