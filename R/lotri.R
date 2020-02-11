.lotri1 <- function(x2, x3, env) {
  env$netas <- length(x3) - 1
  .num <- sqrt(1 + env$netas * 8) / 2 - 1/2
  if (round(.num) == .num) {
    .n <- unlist(strsplit(as.character(x2), " +[+] +"))
    .n <- .n[.n != "+"]
    if (length(.n) == .num) {
      env$names  <- c(env$names, .n)
      .r <- x3[-1]
      .r <- sapply(.r, function(x) {
        return(as.numeric(eval(x)))
      });
      .i <- 0
      .j <- 1
      for (.k in seq_along(.r)) {
        .v <- .r[.k]
        .i <- .i + 1
        if (.i == .j) {
          env$df  <- rbind(env$df,
                           data.frame(i=env$eta1+.i, j=env$eta1+.i, x=.v))
          .j <- .j + 1
          .i <- 0
        } else {
          env$df  <- rbind(env$df,
                           data.frame(i=c(env$eta1+.i, env$eta1+.j),
                                      j=c(env$eta1+.j, env$eta1+.i), x=.v))
        }
      }
      env$eta1 <- env$eta1 + .num
    }  else {
      stop("number of items and lower triangular matrix mismatch")
    }
  } else {
    stop("matrix expression should be 'name ~ c(lower-tri)'")
  }

}

##' Easily Specify block-diagonal matrices with lower triangular info
##'
##' @param x list, matrix or expression, see details
##'
##' @param ... Other arguments treated as a list that will be
##'     concatenated then reapplied to this function.
##'
##' @inheritParams base::eval
##'
##' @return named symmetric matrix useful in RxODE simulations (and
##'     perhaps elsewhere)
##'
##' @details
##'
##'  This can take an R matrix, a list including matrices or
##'  expressions, or expressions
##'
##'  Expressions can take the form
##'
##'  name ~ estimate
##'
##'  Or the lower triangular matrix when "adding" the names
##'
##'  name1 + name2 ~ c(est1,
##'                    est2, est3)
##'
##'  The matricies are concatenated into a block diagonal matrix, like
##'  \code{\link[Matrix]{bdiag}}, but allows expressions to specify
##'  matrices easier.
##'
##'
##' @examples
##'
##' ## A few ways to specify the same matrix
##' lotri({et2 + et3 + et4 ~ c(40,
##'                            0.1, 20,
##'                            0.1, 0.1, 30)})
##'
##' ## You  do not need to enclose in {}
##' lotri(et2 + et3 + et4 ~ c(40,
##'                           0.1, 20,
##'                           0.1, 0.1, 30),
##'           et5 ~ 6)
##' ## But if you do enclose in {}, you can use multi-line matrix specifications:
##'
##' lotri({et2 + et3 + et4 ~ c(40,
##'                            0.1, 20,
##'                            0.1, 0.1, 30);
##'           et5 ~ 6;
##'           })
##'
##' ## You can also add lists or actual R matrices as in this example:
##' lotri(list(et2 + et3 + et4 ~ c(40,
##'                                0.1, 20,
##'                                0.1, 0.1, 30),
##'               matrix(1,dimnames=list("et5","et5"))))
##'
##' ## Overall this is a flexible way to specify symmetric block diagonal matrices.
##'
##' @author Matthew L Fidler
##' @importFrom methods is
##' @export
lotri  <- function(x, ..., envir=parent.frame()) {
  .call <- as.list(match.call())[-1];
  .ncall <- names(.call)
  if (any(.ncall == "envir")) {
    .w <- which(.ncall == "envir")
    .call <- .call[-.w]
  }
  .fullCnd <- NULL
  if (length(.call[[1]]) > 1) {
    if (identical(.call[[1]][[1]], quote(`|`))) {
      .cnd <- .call[[1]][[3]];
      if (length(.cnd) == 1) {
        .fullCnd <- as.character(.cnd);
        x <- eval(.call[[1]][[2]], envir=envir)
      } else {
        stop("unsupported conditioning `|`")
      }
    }
  }
  if (is.null(x)) {
    .ret  <- NULL;
  } else if (is.list(x)) {
    omega  <- lapply(x, lotri);
    if (is(omega, "list")) {
      .omega <- as.matrix(Matrix::bdiag(omega));
      .d <- unlist(lapply(seq_along(omega),
                          function(x) {
                            dimnames(omega[[x]])[2]
                          }))
      dimnames(.omega) <- list(.d, .d);
      omega <- .omega;
    }
    .ret  <- omega
  } else if (is.matrix(x)) {
    .ret  <- x
  } else {
    .env  <- new.env(parent=emptyenv());
    .env$df  <- NULL;
    .env$eta1 <- 0L;
    .env$cnd <- character();
    .f  <- function(x, env) {
      if (is.name(x)) {
        return(character())
      } else if (is.call(x)) {
        if (identical(x[[1]], quote(`~`))) {
          if (length(x[[3]]) == 1) {
            ## et1 ~ 0.2
            env$netas <- 1;
            env$eta1 <- env$eta1 + 1;
            env$names  <- c(env$names, as.character(x[[2]]));
            env$df  <- rbind(env$df,
                             data.frame(i=env$eta1, j=env$eta1, x=as.numeric(eval(x[[3]]))))
          } else {
            ## et1+et2+et3~c() lower triangular matrix
            ## Should fixed be allowed????
            if (any(tolower(as.character(x[[3]][[1]])) == c("c", "fix", "fixed"))) {
              if (any(tolower(as.character(x[[3]][[1]])) == c("fix", "fixed"))){
                stop("fix/fixed are not allowed with lotri matrix specifications")
              }
              .lotri1(x[[2]], x[[3]], env)
            } else {
              .val <- try(eval(x[[3]]), silent=TRUE)
              if (is.numeric(.val) || is.integer(.val)) {
                env$netas <- 1
                env$eta1 <- env$eta1 + 1
                env$names  <- c(env$names, as.character(x[[2]]))
                env$df  <- rbind(env$df,
                                 data.frame(i=env$eta1, j=env$eta1, x=.val))
              } else {
                .cnd <- try(as.character(x[[3]][[1]]), silent=TRUE)
                .didCnd <- FALSE
                if (inherits(.cnd, "character")) {
                  if (.cnd == "|") {
                    .cnd <- x[[3]][[3]]
                    if (length(.cnd) == 1) {
                      .cnd <- as.character(.cnd);
                      ## Each condition is parsed so this new environment should not be elsewhere
                      .env2  <- new.env(parent=emptyenv())
                      .env2$df  <- NULL
                      .env2$eta1 <- 0L
                      env$cnd <- unique(c(env$cnd, .cnd))
                      env[[.cnd]] <- .env2
                      .val <- try(eval(x[[3]][[2]]), silent=TRUE)
                      if ((length(.val) == 1) && (is.numeric(.val) || is.integer(.val))) {
                        .env2$netas <- 1;
                        .env2$eta1 <- .env2$eta1 + 1;
                        .env2$names  <- c(.env2$names, as.character(x[[2]]));
                        .env2$df  <- rbind(.env2$df,
                                           data.frame(i=.env2$eta1, j=.env2$eta1,
                                                      x=.val));
                      } else {
                        .lotri1(x[[2]], x[[3]][[2]], .env2);
                      }
                      .didCnd <- TRUE
                    }
                  }
                }
                if (!.didCnd) stop("matrix expression should be 'name ~ c(lower-tri)'");
              }
            }
          }
        } else if (identical(x[[1]], quote(`{`))) {
          lapply(x, .f, env=env)
        } else if (identical(x[[1]], quote(`quote`))) {
          lapply(x[[2]], .f, env=env)
        } else {
          stop("matrix expression should be 'name ~ c(lower-tri)'")
        }
      } else {
        ## is.pairlist OR is.atomic OR unknown...
        stop("bad matrix specification");
      }
    }
    .sX  <- substitute(x)
    if (is.call(.sX)){
      if (identical(.sX[[1]], quote(`[[`))) {
        .sX  <- x
      }
    }
    .doParse  <- TRUE
    if (.doParse) {
      .f(.sX, .env)
      if (length(.env$cnd) == 0L) {
        .ret <- diag(.env$eta1);
        for (.i in seq_along(.env$df$i)) {
          .ret[.env$df$i[.i], .env$df$j[.i]]  <- .env$df$x[.i];
        }
        dimnames(.ret)  <- list(.env$names, .env$names)
      } else {
        .lstC <- list()
        .other <- NULL
        if (length(.call) > 1) {
          .call <- .call[-1]
          .other <- do.call("lotri", .call, envir=envir);
        }
        for (.j in .env$cnd) {
          .env2 <- .env[[.j]]
          .ret0 <- diag(.env2$eta1)
          for (.i in seq_along(.env2$df$i)) {
            .ret0[.env2$df$i[.i], .env2$df$j[.i]]  <- .env2$df$x[.i]
          }
          dimnames(.ret0)  <- list(.env2$names, .env2$names)
          if (inherits(.other, "list")) {
            if (any(names(.other) == .j)) {
              .ret0 <- do.call("lotri", list(.ret0, .other[[.j]],
                                             envir=envir),
                               envir=envir)
              .other <- .other[names(.other) != .j]
            }
          }
          .lstC[[.j]] <- .ret0
        }
        if (inherits(.other, "list")) {
          .lstC <- c(.lstC, .other)
        } else if (!is.null(.other)) {
          return(c(.lstC, list(.other)))
        }
        return(.lstC)
      }
    }
  }
  if (!is.null(.fullCnd)) {
    .lst <- list()
    .lst[[.fullCnd]] <- .ret
    if (length(.call) == 1L) return(.lst)
    .call <- .call[-1]
    .tmp <- do.call("lotri", .call, envir=envir)
    if (any(names(.tmp) == .fullCnd)) {
      .ret <- lotri(list(.ret, .tmp[[.fullCnd]]), envir=envir);
      .w <- which(names(.tmp) != .fullCnd)
      if (length(.w) > 0L) {
        .tmp <- .tmp[.w];
        .tmp2 <- list();
        .tmp2[[.fullCnd]] <- .ret
        return(c(.tmp2, .tmp));
      } else {
        .tmp <- list();
        .tmp[[.fullCnd]] <- .ret
        return(.tmp)
      }
    } else {
      .lst <- list();
      .lst[[.fullCnd]] <- .ret;
      return(c(.lst, .tmp))
    }
  } else {
    if (length(.call) == 1L) return(.ret)
    .call <- .call[-1]
    .tmp <- do.call("lotri", .call, envir=envir)
    if (inherits(.tmp, "list")){
      if (any(names(.tmp) == "")) {
        .w <- which(names(.tmp) == "")
        .tmp[[.w]] <- do.call("lotri", list(.ret, .tmp[[.w]], envir=envir), envir=envir)
        return(.tmp)
      } else {
        .ret <- c(list(.ret), .tmp)
        return(.ret)
      }
    } else {
      return(lotri(c(list(.ret), list(.tmp)), envir=envir))
    }
  }
}
