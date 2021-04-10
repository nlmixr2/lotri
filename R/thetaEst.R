.deparse1 <- function (expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}

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
  .numeric <- vapply(.ret, is.numeric, logical(1))
  if (!all(.numeric)) {
    return(.deparse1(x))
  } else if (length(.ret) == 1) {
    .ret <- c(-Inf, .ret, Inf)
  } else if (length(.ret) == 2) {
    .ret <- c(.ret, Inf)
  } else if (length(.ret) != 3) {
    return(.deparse1(x))
  }
  data.frame(lower=.ret[1], est=.ret[2], upper=.ret[3], fix=.env$fix,
             stringsAsFactors = FALSE)
}


.parseThetaEstQ <- function(x, env, envir=parent.frame()) {
  if (is.call(x)) {
    if (identical(x[[1]], quote(`{`))) {
      .x <- x[-1]
      .len <- length(.x)
      env$.lines <- character(.len)
      env$.err <- vector("list", .len)
      env$.dfToLine <- NULL
      .lastDfLen <- 0
      for (.i in seq_along(.x)) {
        env$.lines[.i] <- sprintf("\033[1m:%03d\033[0m: %s", .i, .deparse1(.x[[.i]]))
        .parseThetaEstQ(.x[[.i]], env=env, envir=envir)
        if (length(env$err) > 0) {
          env$.err[[.i]] <- paste(env$err, collapse="\n")
          env$err <- NULL
          env$.hasErr <- TRUE
        }
        .dfLen <- length(env$df)
        if (.dfLen != .lastDfLen) {
          .lastDfLen <- .dfLen
          env$.dfToLine <- c(env$.dfToLine, .i)
        }
      }
    } else if (identical(x[[1]], quote(`quote`))) {
      lapply(x[[2]], .parseThetaEstQ, env=env, envir = envir)
    } else if (identical(x[[1]], quote(`label`))) {
      .lab <- ""
      if (is.character(x[[2]])) {
        .lab <- x[[2]]
      } else {
        .lab <- .deparse1(x[[2]])
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
        .fun <- .deparse1(x[[2]])
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

.parseThetaEstBadEsts <- function(env, lines, text) {
  for (i in seq_along(lines)) {
    env$.err[[lines[i]]] <- paste(c(env$err[[lines[i]]], text[i]), collapse="\n")
  }
}

.parseThetaEst <- function(x, envir=parent.frame()) {
  .env <- new.env(parent=emptyenv())
  .env$.hasErr <- FALSE
  .env$df <- NULL
  .env$err <- NULL
  .parseThetaEstQ(x, .env, envir=envir)
  if (!is.null(.env$df)){
    .env$df <- do.call(rbind, .env$df)
    .w <- which(is.na(.env$df$lower))
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("lower bounds cannot be NA: '", .env$df$name[.w], "'"))
    }
    .w <- which(is.na(.env$df$upper))
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("upper bounds cannot be NA: '", .env$df$name[.w], "'"))
    }
    .w <- which(is.na(.env$df$est))
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("initial estimates cannot be NA: '", .env$df$name[.w], "'"))
    }

    .w <- which(is.nan(.env$df$lower))
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("lower bounds cannot be NaN: '", .env$df$name[.w], "'"))
    }
    .w <- which(is.nan(.env$df$upper))
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("upper bounds cannot be NaN: '", .env$df$name[.w], "'"))
    }
    .w <- which(is.nan(.env$df$est))
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("initial estimates cannot be NaN: '", .env$df$name[.w], "'"))
    }

    .w <- which(!is.finite(.env$df$est))
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("initial estimates cannot be infinite: '", .env$df$name[.w], "'"))
    }
    .w <- which(.env$df$lower == Inf)
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("lower bounds cannot be +Inf: '", paste(.env$df$name[.w], collapse="', '"), "'"))
    }
    .w <- which(.env$df$upper == -Inf)
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("upper bounds cannot be -Inf: '", .env$df$name[.w], "'"))
    }

    .w <- which(.env$df$upper == .env$df$est | .env$df$lower == .env$df$est)
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("estimate cannot be equal upper or lower bounds: '", .env$df$name[.w], "'"))
    }

    .w <- which(.env$df$upper < .env$df$est | .env$df$lower > .env$df$est)
    if (length(.w) > 0) {
      .parseThetaEstBadEsts(.env, .env$.dfToLine[.w],
                            paste0("estimate and bounds need to be re-ordered: '", .env$df$name[.w], "'"))
    }
    ## print(.env$.dfToLine)
    ## print(.env$.err)
  }

  if (.env$.hasErr) {
    message("\033[1mlotri syntax error:\n================================================================================\033[0m")
    for (i in seq_along(.env$.err)) {
      if (!is.null(.env$.err[[i]])) {
        message(crayon::bold("lotri error:"))
        .err <- paste(paste("  ", strsplit(.env$.err[[i]], "\n")[[1]]), collapse="\n")
        message(.err)
      }
      message(.env$.lines[i])
    }
    message("\033[1m================================================================================\033[0m")
    ## message(paste(.env$.lines, collapse="\n"))
    stop("lotri syntax errors above", call. = FALSE)
  }
  return(.env$df)
}

##' Extract or remove lotri estimate data frame from lotri object
##'
##' @param x lotri object
##'
##' @param drop boolean indicating if the lotri estimate should be dropped
##'
##' @return data frame with estimates or NULL if there is not a data.frame attached
##'
##' @examples
##'
##' fix1 <- lotri({
##'    a <- c(0, 1); backTransform("exp"); label("a label")
##'    b <- c(0, 1, 2)
##'    c <- fix(1)
##'    d <- fix(0, 1, 2)
##'    e <- c(0, 1, 2, fixed)
##'    f+g ~ c(1,
##'            0.5, 1)
##'  })
##'
##' # Extract the attached lotri estimate data frame
##' lotriEst(fix1)
##'
##' # Remove the attached lotri estimate data frame
##' lotriEst(fix1, drop=TRUE)
##'
##' @export
lotriEst <- function(x, drop=FALSE) {
  if (drop) {
    y <- x
    attr(y, "lotriEst") <- NULL
    if (!is.null(attr(y, "lotriFix"))) {
      return(y)
    }
    class(y) <- NULL
    return(y)
  } else {
    attr(x, "lotriEst")
  }
}
