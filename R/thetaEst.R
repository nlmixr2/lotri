.parseThetaEstFixQ <- function(x, env) {
  x  <- .repFixedWithC(x, env)
  if (is.call(x)) {
    # See if c(1,2,3,fixed) is present.  If so drop the fixed element
    # and flag the environment
    .w <- which(vapply(x, function(y) {
      identical(y, quote(`fixed`)) ||
        identical(y, quote(`fix`)) ||
        identical(y, quote(`Fixed`)) ||
        identical(y, quote(`Fix`)) ||
        identical(y, quote(`FIXED`)) ||
        identical(y, quote(`FIX`))
    }, logical(1)))
    if (length(.w) > 0) {
      env$fix <- TRUE
      x <- x[-.w]
    }
    return(as.call(lapply(x, .parseThetaEstFixQ, env=env)))
  } else {
    return(x)
  }
}

.parseThetaEstFix <- function(x, envir=parent.frame()) {
  .env <- new.env(parent=emptyenv())
  .env$fix <- FALSE
  x <- .parseThetaEstFixQ(x, .env)
  .ret <- as.numeric(eval(x, envir=envir))
  if (length(.ret) == 1) {
    .ret <- c(-Inf, .ret, Inf)
  } else if (length(.ret) == 2) {
    .ret <- c(.ret, Inf)
  } else if (length(.ret) != 3) {
    stop("estimate syntax unsupported: ", deparse(x), call.=FALSE)
  }
  data.frame(lower=.ret[1], est=.ret[2], upper=.ret[3], fix=.env$fix)
}


.parseThetaEstQ <- function(x, env, envir=parent.frame()) {
  if (is.call(x)) {
     if (identical(x[[1]], quote(`{`))) {
       lapply(x[-1],  .parseThetaEstQ, env=env, envir = envir)
     } else if (identical(x[[1]], quote(`quote`))) {
       lapply(x[[2]], .parseThetaEstQ, env=env, envir = envir)
     } else if  (identical(x[[1]], quote(`<-`)) ||
                   identical(x[[1]], quote(`=`))) {

       env$df <- c(env$df, list(data.frame(name=as.character(x[[2]]), .parseThetaEstFix(x[[3]], envir=envir))))
     } else {
      return(as.call(lapply(x, .parseThetaEstQ, env=env, envir=envir)))
    }
  } else {
    return(x)
  }
}

.parseThetaEst <- function(x, envir=parent.frame()) {
  .env <- new.env(parent=emptyenv())
  .env$df <- NULL
  .parseThetaEstQ(x, .env, envir=envir)
  if (!is.null(.env$df)){
    .env$df <- do.call(rbind, .env$df)
  }
  return(.env$df)
}
