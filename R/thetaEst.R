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
  .x <- .parseThetaEstFixQ(x, .env)
  .ret <- try(eval(.x, envir=envir), silent=TRUE)
  if (inherits(.ret, "try-error")) return(deparse1(x))
  .numeric <- vapply(.ret, is.numeric, logical(1))
  if (!all(.numeric)) {
    return(deparse1(x))
  } else if (length(.ret) == 1) {
    .ret <- c(-Inf, .ret, Inf)
  } else if (length(.ret) == 2) {
    .ret <- c(.ret, Inf)
  } else if (length(.ret) != 3) {
    return(deparse1(x))
  }
  data.frame(lower=.ret[1], est=.ret[2], upper=.ret[3], fix=.env$fix,
             stringsAsFactors = FALSE)
}


.parseThetaEstQ <- function(x, env, envir=parent.frame()) {
  if (is.call(x)) {
     if (identical(x[[1]], quote(`{`))) {
       lapply(x[-1],  .parseThetaEstQ, env=env, envir = envir)
     } else if (identical(x[[1]], quote(`quote`))) {
       lapply(x[[2]], .parseThetaEstQ, env=env, envir = envir)
     } else if (identical(x[[1]], quote(`label`))) {
       .lab <- ""
       if (is.character(x[[2]])) {
         .lab <- x[[2]]
       } else {
         .lab <- deparse1(x[[2]])
       }
       .lst <- env$df
       .df0 <- .lst[[length(.lst)]]
       .df0$label <- .lab
       .lst[[length(.lst)]] <- .df0
       env$df <- .lst
     } else if (identical(x[[1]], quote(`backTransform`))) {
       .fun <- ""
       if (is.character(x[[2]])) {
         .fun <- x[[2]]
       } else {
         .fun <- deparse1(x[[2]])
       }
       .lst <- env$df
       .df0 <- .lst[[length(.lst)]]
       .df0$backTransform <- .fun
       .lst[[length(.lst)]] <- .df0
       env$df <- .lst
     } else if  (identical(x[[1]], quote(`<-`)) ||
                   identical(x[[1]], quote(`=`))) {
       .name <- as.character(x[[2]])
       .df <- .parseThetaEstFix(x[[3]], envir=envir)
       if (inherits(.df, "data.frame")) {
         env$df <- c(env$df, list(data.frame(name=.name, .df, label=NA_character_,
                                             backTransform=NA_character_,
                                             stringsAsFactors = FALSE)))
       } else {
         env$err <- c(env$err, paste0("estimate syntax unsupported: ", .name, " ", deparse(x[[1]]), " ", .df))
       }
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
  .env$err <- NULL
  .parseThetaEstQ(x, .env, envir=envir)
  if (!is.null(.env$df)){
    .env$df <- do.call(rbind, .env$df)
    .w <- which(is.na(.env$df$lower))
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("lower bounds cannot be NA: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }
    .w <- which(is.na(.env$df$upper))
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("upper bounds cannot be NA: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }
        .w <- which(is.na(.env$df$est))
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("initial estimates cannot be NA: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }

    .w <- which(is.nan(.env$df$lower))
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("lower bounds cannot be NaN: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }
    .w <- which(is.nan(.env$df$upper))
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("upper bounds cannot be NaN: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }
    .w <- which(is.nan(.env$df$est))
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("initial estimates cannot be NaN: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }

    .w <- which(!is.finite(.env$df$est))
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("initial estimates cannot be infinite: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }
    .w <- which(.env$df$lower == Inf)
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("lower bounds cannot be +Inf: '", paste(.env$df$name[.w], collapse="', '"), "'"))m
    }
    .w <- which(.env$df$upper == -Inf)
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("upper bounds cannot be -Inf: '", paste(.env$df$name[.w], collapse="', '"), "'"))m
    }

    .w <- which(.env$df$upper == .env$df$est | .env$df$lower == .env$df$est)
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("estimate cannot be equal upper or lower bounds: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }

    .w <- which(.env$df$upper < .env$df$est | .env$df$lower > .env$df$est)
    if (length(.w) > 0) {
      .env$err <- c(.env$err, paste("estimate and bounds need to be re-ordered: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }
  }
  ## print(.env$err)
  return(.env$df)
}
