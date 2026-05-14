#' deparse1() for versions of R prior to 4.0.0
#'
#' @inheritParams deparse
#' @return a single string with the deparsed expression
#' @noRd
.deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}
#' Parse the fixed element in lotri estimate syntax
#'
#'  This function looks for the presence of the fixed element in the
#'  lotri estimate syntax and flags the environment if it is present.
#'  It also drops the fixed element from the expression so that it can
#'  be evaluated to a numeric vector.
#'
#' @param x an expression to parse for the fixed element
#' @param env environment to flag if the fixed element is present
#' @return the expression with the fixed element removed
#' @noRd
#' @author Matthew L. Fidler
.parseThetaEstFixQ <- function(x, env) {
  x  <- .repFixedWithC(x, env) # nolint
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
    as.call(lapply(x, .parseThetaEstFixQ, env=env))
  } else {
    x
  }
}
#' Parse the fixed theta estimates
#'
#' @param x an expression to parse for fixed theta estimates
#' @param envir environment to evaluate the expression in
#' @return a data frame with columns lower, est, upper, and fix if the
#'   expression can be evaluated to a numeric vector of length 1, 2,
#'   or 3.  Otherwise the original expression is returned as a string.
#' @noRd
#' @author Matthew L. Fidler
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

#' Parse the lotri estimate syntax
#'
#' This function recursively parses the lotri estimate syntax and builds a
#' data frame with columns name, lower, est, upper, label, and backTransform
#' for each estimate.  It also builds a data frame with the lines of the
#' estimate syntax and any errors that are found in the syntax.
#'
#' @param x an expression to parse for lotri estimate syntax
#'
#' @param env environment to store the data frame of estimates and any
#'   errors found in the syntax.  The environment should have the
#'   following variables:
#'
#' - **df:** a list of data frames with columns name, lower, est, upper
#'
#' - **err:** a list of character vectors with any errors found in the syntax
#'
#' - **assign:** a boolean flag indicating if the current expression is an assignment
#'
#' - **.lines:** a character vector with the lines of the estimate syntax
#'
#' - **.dfToLine:** a numeric vector with the line number
#' corresponding to each row of the data frame of estimates
#'
#' - **.hasErr:** a boolean flag indicating if any errors were found in the syntax
#'
#' The function recursively parses the expression and builds the data
#' frame of estimates and any errors found in the syntax.  It also
#' builds a character vector with the lines of the estimate syntax and
#' a numeric vector with the line number corresponding to each row of
#' the data frame of estimates.
#'
#' @param envir environment to evaluate any expressions in the syntax.
#'   This is used to evaluate any expressions for the initial
#'   estimates, lower bounds, and upper bounds.
#'
#' @return an environment with the following variables:
#'
#' - **df:** a data frame with columns name, lower, est, upper, label,
#'   and backTransform for each estimate found in the syntax
#'
#' - **err:** a character vector with any errors found in the syntax
#'
#' - **.lines:** a character vector with the lines of the estimate syntax
#'
#' - **.dfToLine:** a numeric vector with the line number corresponding to each
#'   row of the data frame of estimates
#'
#' - **.hasErr:** a boolean flag indicating if any errors were found in the
#'   syntax
#'
#' @noRd
#' @author Matthew L. Fidler
.parseThetaEstQ <- function(x, env, envir=parent.frame()) {
  if (is.call(x)) {
    .doAssign <- FALSE
    if (exists("assign", env)) {
      .doAssign <- env$assign
    }
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
      if (.doAssign) {
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
      }
    } else if (identical(x[[1]], quote(`backTransform`))) {
      if (.doAssign) {
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
      }
    } else if  (identical(x[[1]], quote(`<-`)) ||
                  identical(x[[1]], quote(`=`))) {
      env$assign <- TRUE
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
      if (identical(x[[1]], quote(`~`))) {
        env$assign <- FALSE
      }
      as.call(lapply(x, .parseThetaEstQ, env=env, envir=envir))
    }
  } else {
    x
  }
}
#' Parse bad estimates in lotri estimate syntax
#'
#'
#' @param env environmet to store the data frame of estimates and any
#'   errors found in the syntax.
#'
#' @param lines a character vector with the lines of the estimate
#'   syntax
#' @param text a character vector with the error text to add to the
#'   environment for each line
#' @return nothing called for the side effect of adding the error text
#'   to the environment
#' @noRd
#' @author Matthew L. Fidler
.parseThetaEstBadEsts <- function(env, lines, text) {
  for (i in seq_along(lines)) {
    env$.err[[lines[i]]] <- paste(c(env$.err[[lines[i]]], text[i]), collapse="\n")
  }
}
#' Parse the lotri estimate syntax and build a data frame of estimates
#'
#' @param x  an expression to parse for lotri estimate syntax
#' @param envir environment to evaluate any expressions in the syntax.
#' @return new environment for parsing the lotri estimate syntax
#' @noRd
#' @author Matthew L. Fidler
.parseThetaEst <- function(x, envir=parent.frame()) {
  .env <- new.env(parent=emptyenv())
  .env$.hasErr <- FALSE
  .env$df <- NULL
  .env$err <- NULL
  .parseThetaEstQ(x, .env, envir=envir)
  if (!is.null(.env$df)) {
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

    .w <- which(is.infinite(.env$df$est))
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
  }
  .env
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
    y
  } else {
    attr(x, "lotriEst")
  }
}
