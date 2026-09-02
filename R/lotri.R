.lotriEnv <- new.env(parent=emptyenv())
.lotriEnv$lastTilde <- FALSE

#' @importFrom utils assignInMyNamespace
#' @useDynLib lotri, .registration = TRUE
NULL
#' Paste inputNum in lower triangular format to input char
#'
#' @param inputChar Input character expression; ie 'a + b ~ '
#' @param inputParse  Parsed expression to format, should be `c()`
#' @return Formated string with lotri offeset
#' @author Matthew Fidler
#' @examples
#'
#' .pasteLotri("matt+ruth~",quote(c(1,2,3)))
#'
#' .pasteLotri("matt+ruth+kids~",quote(c(1,2,3,4,5,6)))
#' @noRd
.pasteLotri <- function(inputChar, inputParse) {
  .ret <- paste0(inputChar, as.character(inputParse[[1]]), "(")
  .nchar0 <- nchar(.ret)
  .line <- paste0("\n", strrep(" ", .nchar0))
  .i <- 0
  .j <- 1
  for (.k in seq_len(length(inputParse) - 1)) {
    .ret <- paste0(.ret,
                   .deparse1(inputParse[[.k + 1]]), # nolint
                   ifelse(.k == length(inputParse) - 1, ")", ", "))
    .i <- .i + 1
    if (.i == .j && .k != length(inputParse) - 1) {
      .ret <- paste0(.ret, .line)
      .j <- .j + 1
      .i <- 0
    }
  }
  .ret
}

#' lotriMatrix convert numeric vector to matrix
#'
#' @param nv Numeric Vector
#'
#' @param chol boolean indicating if this matrix is a chol matrix
#'
#' @param sd boolean indicating if this is a standard deviation
#'
#' @param cor boolean indicting if this is a correlation matrix
#'
#' @return covariance matrix
#'
#' @author Matthew Fidler
#' @noRd
.lotriMatrix <- function(nv, chol=FALSE, sd=FALSE, cor=FALSE, lhs=NULL) {
  .num <- length(nv)
  .num <- sqrt(1 + .num * 8) / 2 - 1 / 2
  if (round(.num) != .num) {
    .dim <- ceiling(.num)
    .newNum <- ((2 * .dim + 1)^2 - 1)/8
    .extra <- paste(paste0("r", seq_len(.newNum - length(nv))), collapse=",")
    .nv <- .deparse1(nv) # nolint
    .nv <- paste0(substr(.nv, 1, nchar(.nv) - 1), ",", .extra, ")")
    .lhs <- strsplit(.deparse1(lhs), # nolint
                     "[+]")[[1]]
    if (length(.lhs) < .dim) {
      .lhs <- c(.lhs, paste0("v", seq_len(.dim - length(.lhs))))
    }
    .lhs <- paste0("  ", paste(.lhs, collapse="+"), "~")
    .expr <- .pasteLotri(.lhs, eval(parse(text=paste0("quote(", .nv, ")"))))
    stop("lower triangular matrix not correct size\n  did you mean something like:\n", .expr, call. = FALSE)
  }
  .ret <- matrix(nrow=.num, ncol=.num)
  .i <- 0
  .j <- 1
  for (.k in seq_along(nv)) {
    .v <- nv[.k]
    .i <- .i + 1
    if (.i == .j) {
      .ret[.i, .i] <- .v
      .j <- .j + 1
      .i <- 0
    } else {
      .ret[.i, .j] <- .ret[.j, .i] <- .v
      if (chol) .ret[.i, .j] <- 0
    }
  }
  if (chol) {
    .ret <- .ret %*% t(.ret)
    return(.ret)
  }
  if (cor) {
    .d <- diag(.ret)
    if (!sd) {
      .d <- sqrt(.d)
    }
    diag(.ret) <- 1
    if (any(abs(.ret) > 1))
      stop("correlations must be between -1 and 1",
           call.=FALSE)
    .mD <- diag(.d)
    return(.mD %*% .ret %*% .mD)
  }
  if (sd) {
    diag(.ret) <- diag(.ret) ^ 2
  }
  .ret
}
#' Convert to Matrix to lotri vector (internal)
#'
#' @param mat matrix to convert to lotri mat
#'
#' @return lotri numeric vector
#'
#' @author Matthew Fidler
#'
#' @noRd
.lotriMatrixVec <- function(mat) {
  .d <- dim(mat)[1]
  .num <- ((2 * .d + 1)^2 - 1)/8
  .ret <- numeric(.num)
  .i <- 0
  .j <- 1
  for (.k in seq_along(.ret)) {
    .i <- .i + 1
    if (.i == .j) {
      .ret[.k] <- mat[.i, .i]
      .j <- .j + 1
      .i <- 0
    } else {
      .ret[.k] <- mat[.i, .j]
    }
  }
  .ret
}
#'  Is x a fixed element?
#'
#'
#' @param x language expression
#' @return TRUE if x is a fixed element, FALSE otherwise
#' @noRd
#' @author Matthew L. Fidler
.isFixedElt <- function(x) {
  (identical(x, quote(`fix`)) ||
     identical(x, quote(`fixed`)) ||
     identical(x, quote(`Fixed`)) ||
     identical(x, quote(`FIXED`)) ||
     identical(x, quote(`Fix`)) ||
     identical(x, quote(`FIX`)))
}
#' Is the element an unfixed element?
#'
#' @param x language expression
#' @return TRUE if x is an unfixed element, FALSE otherwise
#' @noRd
#' @author Matthew L. Fidler
.isUnfixedElt <- function(x) {
  (identical(x, quote(`unfix`)) ||
     identical(x, quote(`unfixed`)) ||
     identical(x, quote(`Unfixed`)) ||
     identical(x, quote(`UNFIXED`)) ||
     identical(x, quote(`Unfix`)) ||
     identical(x, quote(`UNFIX`)))
}

#' This replaces the `fix` and `unfixed` sort of elements with `c`
#'
#' @param x language expression
#' @param env environment to update with if fixed or unfixed elements are found
#' @return language expression with `fix` and `unfixed` elements replaced with `c`
#' @noRd
#' @author Matthew L. Fidler
.repFixedWithC <- function(x, env=new.env(parent=emptyenv())) {
  if (is.call(x)) {
    if (.isFixedElt(x[[1]])) {
      env$fix <- TRUE
      x[[1]] <- quote(`c`)
      x
    } else if (.isUnfixedElt(x[[1]])) {
      env$unfix <- TRUE
      x[[1]] <- quote(`c`)
      x
    } else {
      as.call(lapply(x, .repFixedWithC, env=env))
    }
  } else {
    x
  }
}
#' The evaluates numeric values but checks for fixed/unfixed flags
#'
#'
#' @param x language object
#' @return a list with the numeric as well as the fix/unfix flags
#' @noRd
#' @author Matthew L. Fidler
.evalAsNumericCheckForFixed <- function(x) {
  .env <- new.env(parent=emptyenv())
  .env$fix <- NA
  .env$unfix <- NA
  .num <- as.numeric(eval(.repFixedWithC(x, .env), envir=.lotriParentEnv))
  list(.num, .env$fix, .env$unfix)
}
#' Assert the proper properties of a lotri matrix (cant mix var, sd ) etc
#'
#'
#' @param x expression
#' @param env environment
#' @return Nothing called for side effects
#' @author Matthew L. Fidler
#' @noRd
.lotriParseMatAssertGoodProps <- function(x, env=NULL) {
  if (identical(x[[1]], quote(`sd`))) {
    if (exists("var", envir=env)) {
      stop("cannot use both 'var' and 'sd' in a block", call.=FALSE)
    }
    env$sd <- TRUE
  }
  if (identical(x[[1]], quote(`var`))) {
    if (exists("sd", envir=env)) {
      stop("cannot use both 'var' and 'sd' in a block", call.=FALSE)
    }
    env$var <- TRUE
  }
  if (identical(x[[1]], quote(`cor`))) {
    if (exists("cov", envir=env)) {
      stop("cannot use both 'cov' and 'cor' in a block", call.=FALSE)
    }
    env$cor <- TRUE
  }
  if (identical(x[[1]], quote(`cov`))) {
    if (exists("cor", envir=env)) {
      stop("cannot use both 'cov' and 'cor' in a block", call.=FALSE)
    }
    env$cov <- TRUE
  }
  if (identical(x[[1]], quote(`chol`))) {
    if (exists("cor", envir=env)   ||
          exists("cov", envir=env) ||
          exists("sd", envir=env)  ||
          exists("var", envir=env)) {
      stop("'chol' has to only be with a single block", call.=FALSE)
    }
    env$chol <- TRUE
  }
}
#' Calculate fixed properties
#'
#' @param x expression
#' @param env environment
#' @return nothing called for side effects
#' @author Matthew L. Fidler
#' @noRd
.lotriParseMatCalcFixProp <- function(x, env=NULL) {
  if (.isFixedElt(x[[1]])) {
    env$globalFix <- TRUE
  }
  if (.isUnfixedElt(x[[1]])) {
    env$globalUnfix <- TRUE
  }
}
#' This parses a matrix
#'
#'
#' @param x language object that is currently being parsed
#' @param env environment where matrix is stored
#' @param noMat boolean indicating if the matrix should be calculated
#'   or not; if TRUE then the vector of values is returned instead of
#'   the matrix
#' @return a list with the matrix (or vector if noMat=TRUE) as well as
#'   the fix/unfix flags
#' @noRd
#' @author Matthew L. Fidler
.lotriParseMat <- function(x, env=NULL, noMat=FALSE) {
  .lotriParseMatAssertGoodProps(x, env)
  .lotriParseMatCalcFixProp(x, env)
  if (identical(x[[1]], quote(`+`)) ||
        identical(x[[1]], quote(`-`)) ||
        identical(x[[1]], quote(`*`)) ||
        identical(x[[1]], quote(`/`)) ||
        identical(x[[1]], quote(`^`))) {
    .r <- list(eval(x, envir=.lotriParentEnv))
  } else if (length(x) == 2) {
    return(.lotriParseMat(x[[2]], env=env, noMat=noMat))
  } else if (length(x) == 1) {
    .r <- x
  } else {
    .r <- x[-1]
  }
  ## chol=FALSE, sd=FALSE, cor=FALSE
  if (!exists("chol", env)) env$chol <- FALSE
  if (!exists("sd", env)) env$sd <- FALSE
  if (!exists("cor", env)) env$cor <- FALSE
  .tmp <- vapply(.r, .evalAsNumericCheckForFixed,
                 list(numeric(1), logical(1), logical(1)))
  env$val <- unlist(.tmp[1, ])
  env$fix <- unlist(.tmp[2, ])
  env$unfix <- unlist(.tmp[3, ])
  if (noMat) {
    env$nv <- env$val
  } else if (length(env$lhs) == 1 &&
               length(env$val) != 1) {
    env$nv <- env$val
  } else {
    env$nv <- .lotriMatrixVec(.lotriMatrix(env$val, chol=env$chol, sd=env$sd, cor=env$cor, lhs=env$lhs))
  }
  if (!exists("globalFix", env)) {
    env$globalFix <- FALSE
  }
  if (!exists("globalUnfix", env)) {
    env$globalUnfix <- FALSE
  }
  .fix <- vapply(env$fix, function(x) {
    ifelse(is.na(x), env$globalFix, x)
  }, logical(1))
  .unfix <- vapply(env$unfix, function(x) {
    ifelse(is.na(x), env$globalUnfix, x)
  }, logical(1))
  list(env$nv, .fix, .unfix)
}

#' Handle Matrix Row for Lotri
#'
#' This internal function processes a matrix row for the Lotri package.
#'
#' @param k Integer. The starting index for the row.
#' @param j Integer. The row number to process.
#' @param value Numeric vector. The values to be inserted into the matrix.
#' @param fix Logical vector. Indicates which values are fixed.
#' @param unfix Logical vector. Indicates which values are not fixed.
#' @param env Environment. The environment containing the data frame `df` and the offset `eta1`.
#'
#' @return Integer. The next index to process.
#' @keywords internal
#' @noRd
.lotri1handleMatrixRow <- function(k, j, value, fix, unfix, env) {
  .i <- 0
  .k <- k
  while (TRUE) {
    .v <- value[.k]
    .f <- fix[.k]
    .u <- unfix[.k]
    names(.v) <- names(.f) <- names(.u) <- NULL
    .i <- .i + 1
    .k <- .k + 1
    if (.i == j) {
      env$df <- rbind(
        env$df,
        data.frame(i = env$eta1 + .i, j = env$eta1 + .i, x = .v, fix=.f, unfix=.u)
      )
      return(.k)
    } else {
      env$df <- rbind(
        env$df,
        data.frame(
          i = c(env$eta1 + .i, env$eta1 + j),
          j = c(env$eta1 + j, env$eta1 + .i), x = .v,
          fix=.f, unfix=.u
        )
      )
    }
  }
  NA_integer_
}
#' Handle last expression is a condition for Form #2
#'
#' @param x2 Second element of parsing list ie  `x` or `x1+x2`
#'
#' @param x3 Third element of list; ie c(...)
#' @param env Environment for the current parsing; this is updated if
#'   the last expression is a condition.
#' @return TRUE if the last expression is a condition and the
#'   environment was updated, FALSE otherwise
#' @noRd
#' @author Matthew L. Fidler
.handleLastExprIsCndForFrm2 <- function(x2, x3, env) {
  if (exists("lastCnd", env)) {
    .cnd <- env$lastCnd
    if (exists(.cnd, env)) {
      .env2 <- env[[.cnd]]
      .env2$lastN <- max(.env2$df$i)
      .len <- length(.env2$df$i)
      .lotri1(x2, x3, .env2)
      if (.len < length(.env2$df$i)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Resets the last N if needed (used for multi-line expressions in form #2)
#'
#' eta1 ~ 0
#' eta2 ~ c(0, 1)
#'
#' @param env Reset the last N for the etas
#' @param i the reset number to set lastN to
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.resetLastN <- function(env, i=1L) {
  if (env$lastN > 1L) {
    env$eta1 <- env$eta1 + env$lastN - 1L
  }
  env$lastN <- i
}

#' Handle Single Line Estimation in Form #2
#'
#' This function processes a single line estimation in a form,
#' updating the environment's data frame with the provided values,
#' fixed, and unfixed parameters.
#'
#' This is for lotri matrices of the form x ~ 1; x2 ~ c(0.1, 1); x3 ~
#' c(0.1, 0.2, 1)
#'
#' @param x2 A single element to be processed.
#' @param values A vector of values to be added to the data frame.
#' @param fixed A vector of fixed parameters corresponding to the
#'   values.
#' @param unfixed A vector of unfixed parameters corresponding to the
#'   values.
#' @param env An environment containing the data frame (`df`), the
#'   last number of elements (`lastN`), and other necessary variables.
#'
#' @return Returns `TRUE` if the processing is successful and the data
#'   frame is updated, otherwise returns `FALSE`.
#' @noRd
.handleSingleLineEstInLineForm <- function(x2, values, fixed, unfixed, env) {
  .r <- values
  .rf <- fixed
  .ru <- unfixed
  if (env$lastN != 0 && length(x2) == 1L) {
    if (length(.r) == env$lastN + 1) {
      for (.i in seq_len(env$lastN)) {
        .v <- .r[.i]
        .f <- .rf[.i]
        .u <- .ru[.i]
        names(.v) <- names(.f) <- names(.u) <- NULL
        env$df <- rbind(
          env$df,
          data.frame(
            i = c(env$eta1 + .i-1, env$eta1 + env$lastN),
            j = c(env$eta1 + env$lastN, env$eta1 + .i-1), x = .v,
            fix=.f, unfix=.u
          )
        )
      }
      .v <- .r[env$lastN+1]
      .f <- .rf[env$lastN+1]
      .u <- .ru[env$lastN+1]
      names(.v) <- names(.f) <- names(.u) <- NULL
      env$df <- rbind(
        env$df,
        data.frame(
          i = env$eta1 + env$lastN,
          j = env$eta1 + env$lastN, x = .v,
          fix=.f, unfix=.u
        )
      )
      env$lastN <- env$lastN + 1
      .lotriSameSetBlk(env, env$eta1 - 1L, env$lastN)
      env$names <- c(env$names, deparse1(x2))
      env$labels <- c(env$labels, NA_character_)
      return(TRUE)
    }
  }
  FALSE
}

#' Parse lower triangular matrix list
#'
#' This is for x~c(1..) or x1+x2~c(...)
#'
#' @param x2 Second element of parsing list ie  `x` or `x1+x2`
#'
#' @param x3 Third element of list; ie c(...)
#'
#' @param env  environment to update
#'
#' @param env2 environment to try if the last expression could be a
#'   multi-line expression
#'
#' @return Nothing; updates environment
#'
#' @author Matthew Fidler
#' @noRd
.lotri1 <- function(x2, x3, env, env2=NULL) {
  .envParse <- new.env(parent = emptyenv())
  .envParse$lhs <- x2
  .rl <- .lotriParseMat(x3, env=.envParse)
  .r <- .rl[[1]]
  .rf <- .rl[[2]]
  .ru <- .rl[[3]]
  if (.handleSingleLineEstInLineForm(x2, values=.r, fixed=.rf, unfixed=.ru, env)) {
    return(NULL)
  }
  env$netas <- length(.r)
  .num <- sqrt(1 + env$netas * 8) / 2 - 1 / 2
  if (round(.num) == .num) {
    if (.num == 1) {
      env$lastN <- 1
    }
    .n <- unlist(strsplit(as.character(x2), " +[+] +"))
    .n <- .n[.n != "+"]
    if (length(.n) == .num) {
      env$names <- c(env$names, .n)
      env$labels <- c(env$labels, rep(NA_character_, length(.n)))
      .j <- 1
      .k <- 1
      while (TRUE) {
        .k <- .lotri1handleMatrixRow(k=.k, j=.j, value=.r,
                                     fix=.rf, unfix=.ru, env=env)
        .j <- .j + 1
        if (.k > length(.r)) {
          break
        }
      }
      .lotriSameSetBlk(env, env$eta1, .num)
      env$eta1 <- env$eta1 + .num
    } else if (.num - length(.n) < 0) {
      if (.handleLastExprIsCndForFrm2(x2, x3, env)) {
        return(invisible())
      }
      .expr <- paste(.deparse1(x2), # nolint
                     "~", .deparse1(x3))
      stop("number named variables and lower triangular matrix size do not match:\n",
           .expr)
    } else {
      ## in this case
      if (.handleLastExprIsCndForFrm2(x2, x3, env)) {
        return(invisible())
      }
      .expr <- paste0("quote(",
                      paste(c(.n,
                              paste0("varName", length(.n) + seq_len(.num - length(.n)))),
                            collapse="+"), "~ 0)")
      .expr <- eval(parse(text=.expr))
      .expr <- .deparse1(.expr) # nolint
      .expr <- paste0("  '", substr(.expr, 1, nchar(.expr) - 1))
      .expr <- .pasteLotri(.expr, x3)
      stop("number named variables and lower triangular matrix size do not match\n  did you mean something like:\n", .expr, call. = FALSE) # nolint
    }
  } else {
    if (.handleLastExprIsCndForFrm2(x2, x3, env)) {
      return(invisible())
    }
    if (!is.null(env2)) {
      return(.lotri1(x2, x3, env2))
    }
    stop("matrix expression should be 'name ~ c(lower-tri)'", call. = FALSE)
  }
}

#' Handle Tilde LHS Sum for Lotri
#'
#' This internal function processes the left-hand side of a tilde
#' expression for the Lotri package.  These are used as the names of the matrix.
#'
#' ie x + y + z ~ ...
#'
#' @param x Expression. The expression to be evaluated.
#'
#' @param env Environment. The environment containing the necessary
#'   variables and data frames.
#'
#' @return None. The function modifies the environment `env` by adding to its data frame `df` and other variables.
#' @keywords internal
#' @noRd
.fcallTildeLhsSum <- function(x, env) {
  ## et1+et2+et3~NULL lower triangular matrix
  if (any(tolower(as.character(x[[3]][[1]])) ==
            c("c", "fix", "fixed", "unfix", "unfixed", "var", "sd", "cor", "cov", "chol"))) {
    .lotri1(x[[2]], x[[3]], env)
  } else {
    .val <- try(eval(x[[3]], envir=.lotriParentEnv), silent = TRUE)
    names(.val) <- NULL
    if (is.numeric(.val) || is.integer(.val)) {
      env$netas <- 1
      env$eta1 <- env$eta1 + 1
      .lotriSameSetBlk(env, env$eta1 - 1L, 1L)
      env$names <- c(env$names, as.character(x[[2]]))
      env$labels <- c(env$labels, NA_character_)
      env$df <- rbind(
        env$df,
        data.frame(i = env$eta1, j = env$eta1, x = .val, fix=FALSE, unfix=FALSE)
      )
    } else {
      .cnd <- try(as.character(x[[3]][[1]]), silent = TRUE)
      .didCnd <- FALSE
      if (inherits(.cnd, "character")) {
        if (.cnd == "|") {
          .cnd <- x[[3]][[3]]
          .cndFull <- .parseCondition(.cnd, envir = env)
          .cnd <- .cndFull[[1]]
          if (exists("lastCnd", env)) {
            if (env$lastCnd == .cnd) {
              if (exists(.cnd, env)) {
                .lotri1(x[[2]], x[[3]][[2]], env[[.cnd]], env)
              } else {
                .lotri1(x[[2]], x[[3]][[2]], env)
              }
              return(invisible())
            }
          }
          ## Each condition is parsed so this new environment
          ## should not be elsewhere
          .env2 <- new.env(parent = emptyenv())
          .env2$isCov <- env$isCov
          .env2$rcm  <- env$rcm
          .env2$df <- NULL
          .env2$eta1 <- 0L
          .env2$lastN <- 0L
          env$cnd <- unique(c(env$cnd, .cnd))
          env$lastCnd <- .cnd
          env[[.cnd]] <- .env2
          env[[paste0(.cnd, ".extra")]] <- .cndFull[[2]]
          .val <- .lotriParseMat(x[[3]][[2]], env=env, noMat=TRUE)
          .fix <- .val[[2]]
          .unfix <- .val[[3]]
          .val <- .val[[1]]
          if (length(.val) >= 2L &&
                length(.val) == env$lastN+1) {
            .env2$df <- env$df
            .env2$eta1 <- env$eta1
            .env2$lastN <- env$lastN
            .env2$names <- env$names
            .env2$labels <- env$labels
            .env2$sameOff <- env$sameOff
            .env2$sameBlkN <- env$sameBlkN
            .env2$sameMasterBase <- env$sameMasterBase
            # moved to .env2 for parsing
            env$df <- NULL
            env$lastN <- 0
            env$eta1 <- 0
            env$names <- character(0)
            env$sameOff <- NULL
            env$sameBlkN <- NULL
            env$sameMasterBase <- NULL
            .lotri1(x[[2]], x[[3]][[2]], .env2)
          } else if ((length(.val) == 1) &&
                       (is.numeric(.val) || is.integer(.val))) {
            .env2$netas <- 1L
            .env2$eta1 <- .env2$eta1 + 1L
            .lotriSameSetBlk(.env2, .env2$eta1 - 1L, 1L)
            .env2$names <- c(.env2$names, as.character(x[[2]]))
            .env2$labels <- c(.env2$labels, NA_character_)
            .env2$df <- rbind(.env2$df,
                              data.frame(i = .env2$eta1, j = .env2$eta1,
                                         x = .val,
                                         fix=.fix, unfix=.unfix))
          } else {
            .lotri1(x[[2]], x[[3]][[2]], .env2)
          }
          .didCnd <- TRUE
        }
      }
      if (!.didCnd) {
        stop("bad matrix expression: '",
             .deparse1(x), # nolint
             "'\n  matrix expression should be 'name ~ c(lower-tri)'",
             call. = FALSE)
      }
    }
  }
}
#' Record the block that a following `same()` should repeat
#'
#' `same()` repeats the immediately preceding *block*, which the parse
#' environment does not otherwise remember.  Every site that opens or
#' extends a block calls this with the index of the block's first eta
#' minus one (`base`) and the block's dimension (`n`).
#'
#' The block is tracked explicitly rather than inferred from `env$df`
#' connectivity because `a + b ~ c(1, 0, 1)` is a declared 2x2 with a
#' structural zero covariance; connectivity would misread it as two 1x1
#' blocks.  NONMEM `BLOCK SAME` repeats the *declared* block regardless
#' of zeros.
#'
#' @param env parse environment to update
#' @param base index of the first eta of the block, minus one
#' @param n dimension of the block
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.lotriSameSetBlk <- function(env, base, n) {
  env$sameMasterBase <- base
  env$sameBlkN <- n
  invisible()
}

#' Pad the `same()` offset vector out to the number of parsed names
#'
#' The offsets are only appended by `.fCallSame()`, so rather than
#' touching every site that appends a name this pads lazily with `0L`
#' (meaning "not a repeated block") right before the vector is read.
#'
#' @param env parse environment to update
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.lotriSamePad <- function(env) {
  .n <- length(env$names)
  .cur <- env$sameOff
  if (is.null(.cur)) .cur <- integer(0)
  if (length(.cur) < .n) {
    .cur <- c(.cur, rep(0L, .n - length(.cur)))
  }
  env$sameOff <- .cur
  invisible()
}

#' Match the right hand side of a `~ same()` line
#'
#' @param r right hand side language object
#' @return `NULL` when this is not a `same()` right hand side, otherwise
#'   a list with `cnd` (the conditioning language object, or `NULL`),
#'   `call` (the `same(...)` call itself, so its arity can be checked
#'   with a message that names `same()` rather than falling through to
#'   the generic "bad matrix expression"), and `bad` (a spelling that is
#'   clearly meant to be `same()` but cannot work, reported by
#'   `.fCallSame()` instead of the message the generic path would give)
#' @noRd
#' @author Matthew L. Fidler
.lotriSameRhs <- function(r) {
  if (is.call(r) && identical(r[[1]], quote(`same`))) {
    return(list(cnd=NULL, call=r, bad=NULL))
  }
  if (is.call(r) && length(r) == 3L && identical(r[[1]], quote(`|`)) &&
        is.call(r[[2]]) && identical(r[[2]][[1]], quote(`same`))) {
    return(list(cnd=r[[3]], call=r[[2]], bad=NULL))
  }
  ## `fix(same())` would otherwise be evaluated by `.fCallTilde()` OUTSIDE
  ## a `try()` and die with `could not find function "same"`
  if (is.call(r) && length(r) == 2L &&
        (.isFixedElt(r[[1]]) || .isUnfixedElt(r[[1]])) &&
        is.call(r[[2]]) && identical(r[[2]][[1]], quote(`same`))) {
    return(list(cnd=NULL, call=r[[2]], bad="fix"))
  }
  ## a bare `same` is a missing pair of parentheses -- unless the user
  ## really does have a variable of that name, in which case the old
  ## behaviour (resolve it from the calling frame) is kept
  if (is.name(r) && identical(r, quote(`same`)) &&
        !exists("same", envir=.lotriParentEnv)) {
    return(list(cnd=NULL, call=NULL, bad="bare"))
  }
  NULL
}

#' Is this expression a `name(s) ~ same()` line?
#'
#' @param x language object to test
#' @return TRUE when this repeats the preceding block
#' @noRd
#' @author Matthew L. Fidler
.lotriIsSameLine <- function(x) {
  is.call(x) && length(x) == 3L && identical(x[[1]], quote(`~`)) &&
    !is.null(.lotriSameRhs(x[[3]]))
}

#' Handle a `name(s) ~ same()` line
#'
#' This is NONMEM's `$OMEGA BLOCK(n) SAME`: the block declared just
#' before is stamped again under new names, and the copy shares the
#' master's estimates rather than getting its own.
#'
#' @param x the `~` language object
#' @param env parse environment
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.fCallSame <- function(x, env) {
  .same <- .lotriSameRhs(x[[3]])
  if (identical(.same$bad, "bare")) {
    stop("did you mean 'same()'?", call.=FALSE)
  }
  if (identical(.same$bad, "fix")) {
    stop("'same()' cannot be combined with 'fix()'; a repeated block ",
         "inherits the fixed flags of the block it repeats", call.=FALSE)
  }
  if (length(.same$call) != 1L) {
    stop("'same()' takes no arguments", call.=FALSE)
  }
  .n <- .lotriTildeLhsNames(x[[2]])
  if (is.null(.n)) {
    stop("the left hand side of 'same()' must be parameter name(s)",
         call.=FALSE)
  }
  .tgt <- env
  .at <- ""
  if (!is.null(.same$cnd)) {
    .cnd <- .parseCondition(.same$cnd, envir=env)[[1]]
    .at <- paste0(" at level '", .cnd, "'")
    if (exists("lastCnd", env) && env$lastCnd == .cnd && exists(.cnd, env)) {
      .tgt <- env[[.cnd]]
    } else {
      stop("'same()' has no block to repeat", .at, call.=FALSE)
    }
  }
  if (is.null(.tgt$sameBlkN) || is.null(.tgt$sameMasterBase)) {
    ## Also the message for the `lotri(a ~ 1, b ~ same())` form, where a
    ## preceding block does exist but in a different argument: each
    ## argument is parsed by its own `lotri()` call, so there is no
    ## shared parse state for `same()` to look back into.
    stop("'same()' has no block to repeat", .at,
         "; it must follow a matrix block in the same '{}' block",
         call.=FALSE)
  }
  .blkN <- .tgt$sameBlkN
  if (length(.n) != .blkN) {
    stop("'same()' repeats the previous ", .blkN, "x", .blkN,
         " block, so it needs ", .blkN, " name",
         ifelse(.blkN == 1L, "", "s"), " on the left, not ", length(.n),
         call.=FALSE)
  }
  ## a line-form master leaves `eta1` lagging behind the block it wrote
  .resetLastN(.tgt)
  .base <- .tgt$eta1
  .m <- .tgt$sameMasterBase
  .off <- .base - .m
  .w <- which(.tgt$df$i > .m & .tgt$df$i <= .m + .blkN &
                .tgt$df$j > .m & .tgt$df$j <= .m + .blkN)
  .cp <- .tgt$df[.w, , drop=FALSE]
  .cp$i <- .cp$i + .off
  .cp$j <- .cp$j + .off
  ## `env$df` already carries both symmetric entries for an off diagonal,
  ## so a verbatim copy stays symmetric with no special casing
  .tgt$df <- rbind(.tgt$df, .cp)
  ## pad BEFORE the new names are appended, so the padding covers only
  ## the names parsed so far
  .lotriSamePad(.tgt)
  .tgt$names <- c(.tgt$names, .n)
  .tgt$labels <- c(.tgt$labels, rep(NA_character_, length(.n)))
  .tgt$sameOff <- c(.tgt$sameOff, rep(as.integer(.off), length(.n)))
  .tgt$netas <- .blkN
  .tgt$eta1 <- .base + .blkN
  ## NOT 1L: with 1L a following `x ~ c(0.1, 1)` is taken by
  ## `.handleSingleLineEstInLineForm()` as covarying with the last row of
  ## the copy, which silently corrupts the repeated block
  .tgt$lastN <- 0L
  ## `sameMasterBase`/`sameBlkN` are deliberately left alone so a further
  ## `same()` repeats the ORIGINAL block, the way NONMEM chains `SAME`
  invisible()
}

#' Is this a known call for the fixed/unfixed elements and other functions
#'
#'
#' @param x language expression to check
#' @return TRUE if this is a known call for the fixed/unfixed elements and other functions, FALSE otherwise
#' @noRd
#' @author Matthew L. Fidler
.isKnownCall <- function(x) {
  if (is.call(x) && length(x) >= 1) {
    return(tolower(as.character(x[[1]])) %in%
             c("fix", "fixed", "unfix", "unfixed", "var", "sd", "cor", "cov", "chol"))
  }
  FALSE
}

#' Handle Matrix Expressions with Tilde
#'
#' This function processes matrix expressions of the form `name ~ c(lower-tri)`.
#' It validates the expression, evaluates it, and updates the environment with
#' the results.
#'
#' @param x A language object representing the expression to be evaluated.
#' @param env An environment where the results of the evaluation will be stored.
#'
#' @details
#' The function performs the following steps:
#' 1. Checks if the length of `x` is 3. If not, it attempts to provide a helpful
#'    error message suggesting the correct format.
#' 2. If the right-hand side of the expression (`x[[3]]`) is a single name and
#'    exists in the parent environment, it evaluates and replaces it.
#' 3. If the right-hand side is a single numeric value, it updates the environment
#'    with the new matrix element.
#' 4. If the right-hand side is more complex, it delegates to another function
#'    `.fcallTildeLhsSum`.
#'
#' @return This function does not return a value. It updates the provided environment.
#'
#' @noRd
.fCallTilde <- function(x, env) {
  if (length(x) != 3) {
    .possible <- paste("quote(variableName",
                       .deparse1(x), # nolint
                       ")")
    .possible <- try(.deparse1(eval(parse(text=.possible))), silent=TRUE) # nolint
    .err <- "matrix expression should be 'name ~ c(lower-tri)'"
    if (!inherits(.possible, "try-error")) {
      .err <- paste0(.err, "\n  did you mean '", .possible, "'")
    }
    stop(.err, call. = FALSE)
  }
  if (length(x[[3]]) == 1L &&
        is.name(x[[3]]) &&
        exists(as.character(x[[3]]), envir=.lotriParentEnv)) {
    x[[3]] <- str2lang(deparse1(get(as.character(x[[3]]), envir=.lotriParentEnv)))
  }
  .fix <- FALSE
  .unfix <- FALSE
  .x3 <- x[[3]]
  if (length(.x3) == 2L &&
        identical(.x3[[1]], quote(`c`))) {
    .x3t <- eval(.x3, envir=.lotriParentEnv)
    if (length(.x3t) == 1L && is.numeric(.x3t)) {
      .x3 <- .x3t
    }
  } else if (length(.x3) == 2L &&
               .isFixedElt(.x3[[1]]) &&
               !.isKnownCall(.x3[[2]])) {
    .x3t <- .x3
    .x3t[[1]] <- quote(`c`)
    .x3t <- eval(.x3t, envir=.lotriParentEnv)
    if (length(.x3t) == 1L && is.numeric(.x3t)) {
      .x3 <- .x3t
      .fix <- TRUE
    }
  } else if (length(.x3) == 2L &&
               .isUnfixedElt(.x3[[1]]) &&
               !.isKnownCall(.x3[[2]])) {
    .x3t <- .x3
    .x3t[[1]] <- quote(`c`)
    .x3t <- eval(.x3t, envir=.lotriParentEnv)
    if (length(.x3t) == 1L && is.numeric(.x3t)) {
      .x3 <- .x3t
      .unfix <- TRUE
    }
  } else if (length(.x3) == 2L &&
               !.isKnownCall(.x3)) {
    .x3t <- try(eval(.x3, envir=.lotriParentEnv), silent=TRUE)
    if (!inherits(.x3t, "try-error") &&
          length(.x3t) == 1L && is.numeric(.x3t)) {
      .x3 <- .x3t
    }
  }
  if (length(.x3) == 1) {
    .resetLastN(env)
    ## et1 ~ 0.2
    if (is.numeric(.x3)) {
      env$lastN <- 1
      env$netas <- 1
      env$eta1 <- env$eta1 + 1
      .lotriSameSetBlk(env, env$eta1 - 1L, 1L)
      env$names <- c(env$names, as.character(x[[2]]))
      env$labels <- c(env$labels, NA_character_)
      env$df <- rbind(env$df,
                      data.frame(i = env$eta1,
                                 j = env$eta1,
                                 x = setNames(eval(.x3, envir=.lotriParentEnv), NULL),
                                 fix=.fix, unfix=.unfix))
    } else {
      stop("cannot figure out expression `", deparse1(x), "` in lotri while handling `~`")
    }
  } else {
    .fcallTildeLhsSum(x, env)
  }
}
#' Is this expression a `prior(name) ~ dist(...)` line?
#'
#' @param x language object to test
#' @return TRUE when this is a prior specification line
#' @noRd
#' @author Matthew L. Fidler
.lotriIsPriorLine <- function(x) {
  is.call(x) && length(x) == 3L &&
    identical(x[[1]], quote(`~`)) &&
    is.call(x[[2]]) && identical(x[[2]][[1]], quote(`prior`))
}

#' Collect a `prior(name) ~ dist(...)` line
#'
#' The prior is validated here (so syntax errors are reported with the
#' line they occurred on) but it is *resolved* against the parameters
#' later, once the matrix and any `rcm` re-ordering are complete.
#'
#' @param x language object of the prior line
#' @param env parsing environment
#' @return nothing, called for the side effect on `env$priors`
#' @noRd
#' @author Matthew L. Fidler
#' Is the right hand side of a prior a distribution call?
#'
#' Anything that is not a distribution is taken as a variance
#' specification, ie the normal prior shorthand written under `prior()`.
#'
#' @param x right hand side language object
#' @return TRUE when it names a supported distribution
#' @noRd
#' @author Matthew L. Fidler
.lotriPriorRhsIsDist <- function(x) {
  .nm <- NULL
  if (is.name(x)) {
    .nm <- as.character(x)
  } else if (is.call(x)) {
    .h <- as.character(x[[1]])
    if (length(.h) == 1L) .nm <- .h
  }
  if (is.null(.nm)) return(FALSE)
  !is.null(.lotriPriorLookup(.nm))
}

.fCallPrior <- function(x, env) {
  .lhs <- as.list(x[[2]])[-1]
  if (length(.lhs) == 0L) {
    stop("'prior()' requires at least one parameter name", call.=FALSE)
  }
  .nm <- vapply(.lhs, function(y) {
    if (is.name(y)) return(as.character(y))
    if (is.character(y) && length(y) == 1L) return(y)
    stop("'prior()' arguments must be parameter names, not '",
         .deparse1(y), "'", call.=FALSE) # nolint
  }, character(1), USE.NAMES=FALSE)
  .dup <- unique(.nm[duplicated(.nm)])
  if (length(.dup) > 0) {
    stop("duplicated parameter(s) in 'prior()': '", paste(.dup, collapse="', '"), "'",
         call.=FALSE)
  }
  ## `prior(tka) ~ stats::dnorm(0, 1)` would otherwise fall through to
  ## the shorthand below and be *evaluated*, silently becoming a variance
  ## of 0.24 rather than the distribution that was plainly meant
  .rhs <- x[[3]]
  if (is.call(.rhs) && is.call(.rhs[[1]]) && length(.rhs[[1]]) == 3L &&
        (identical(.rhs[[1]][[1]], quote(`::`)) ||
           identical(.rhs[[1]][[1]], quote(`:::`)))) {
    .fn <- as.character(.rhs[[1]][[3]])
    if (length(.fn) == 1L && !is.null(.lotriPriorLookup(.fn))) {
      stop("a prior distribution is not namespaced; write '", .fn,
           "(...)' rather than '", .deparse1(.rhs[[1]]), "(...)'", # nolint
           call.=FALSE)
    }
  }
  if (!.lotriPriorRhsIsDist(x[[3]])) {
    ## `prior(tka) ~ 0.1` is the normal prior shorthand written under a
    ## `prior()`.  In a block `tka ~ 0.1` already means this, but the
    ## `prior()` form works where a bare `~` cannot: piping onto a model,
    ## where `tka ~ 0.1` has always meant "change the estimate".
    ##
    ## Fed through the ordinary matrix parser into one accumulating
    ## environment, so consecutive lines build a block the way the bare
    ## line form does: `prior(tcl) ~ 1; prior(tv) ~ c(0.001, 1)` is the
    ## same 2x2 as `tcl ~ 1; tv ~ c(0.001, 1)`.  Note this is the one
    ## place a prior line is *not* order independent, because the row
    ## form has to lean on the line before it.
    if (is.null(env$priorShorthandEnv)) {
      env$priorShorthandEnv <- .lotriNewPriorEnv()
    }
    .fCallTilde(as.call(list(quote(`~`),
                             str2lang(paste(.nm, collapse=" + ")),
                             x[[3]])),
                env$priorShorthandEnv)
    return(invisible())
  }
  env$priors <- c(env$priors,
                  list(list(names=.nm, info=.lotriPriorNormalize(x[[3]]))))
  invisible()
}

#' Names on the left hand side of a `~`
#'
#' @param x left hand side language object, ie `a` or `a + b + c`
#' @return character vector of names, or NULL when the left hand side is
#'   not a name or a sum of names
#' @noRd
#' @author Matthew L. Fidler
.lotriTildeLhsNames <- function(x) {
  if (is.name(x)) return(as.character(x))
  if (is.call(x) && identical(x[[1]], quote(`+`)) && length(x) == 3L) {
    .l <- .lotriTildeLhsNames(x[[2]])
    .r <- .lotriTildeLhsNames(x[[3]])
    if (is.null(.l) || is.null(.r)) return(NULL)
    return(c(.l, .r))
  }
  NULL
}

#' Every plain (non-`om.`) name on the left of a `~` anywhere in a block
#'
#' The omega prior shorthand's target has to already be a real eta, but
#' a block does not have to declare that eta before the `om.` line that
#' targets it -- `prior()` lines are explicitly order independent, and
#' there is nothing that makes the shorthand different.  This walks the
#' whole block once up front (mirroring `.parseThetaEst()`'s up-front
#' pass for population estimates) so the check does not depend on
#' whether the eta happens to have been parsed yet.
#'
#' @param x language object, typically the whole `{...}` block
#' @return character vector of every plain eta name declared anywhere
#'   in `x`
#' @noRd
#' @author Matthew L. Fidler
.lotriAllEtaLhsNames <- function(x) {
  .names <- character(0)
  .walk <- function(y) {
    if (!is.call(y)) return(invisible())
    if (identical(y[[1]], quote(`{`))) {
      for (.i in seq_along(y)[-1]) .walk(y[[.i]])
    } else if (identical(y[[1]], quote(`~`)) && length(y) == 3L) {
      .nm <- .lotriTildeLhsNames(y[[2]])
      if (!is.null(.nm)) {
        .names <<- c(.names, .nm[!grepl("^om[.].", .nm)])
      }
    }
  }
  .walk(x)
  unique(.names)
}

#' Is this a `~invWishart(4)` whole omega prior line?
#'
#' A one sided `~` with a matrix valued distribution applies that prior
#' to every block of the omega, which saves naming each block when they
#' all share the same degrees of freedom.
#'
#' @param x language object to test
#' @return TRUE when this is a whole omega prior
#' @noRd
#' @author Matthew L. Fidler
.lotriIsWholeOmegaPriorLine <- function(x) {
  if (!(is.call(x) && length(x) == 2L && identical(x[[1]], quote(`~`)))) {
    return(FALSE)
  }
  .r <- x[[2]]
  if (!is.call(.r)) return(FALSE)
  .nm <- as.character(.r[[1]])
  if (length(.nm) != 1L) return(FALSE)
  .d <- .lotriPriorLookup(.nm)
  ## only the matrix valued distributions; `~c(40)` stays an error
  !is.null(.d) && .d$kind == "matrix"
}

#' Collect a `~invWishart(4)` whole omega prior
#'
#' @param x language object of the prior line
#' @param env parsing environment
#' @return nothing, called for the side effect on `env$wholeOmegaPrior`
#' @noRd
#' @author Matthew L. Fidler
.fCallWholeOmegaPrior <- function(x, env) {
  env$wholeOmegaPrior <- c(env$wholeOmegaPrior,
                           list(.lotriPriorNormalize(x[[2]])))
  invisible()
}

#' Strip the `om.` prefix used to name an omega element
#'
#' In a NONMEM `TNPRI` model the prior is over the omega elements as
#' well as the thetas, so the elements need names of their own.  `om.`
#' prepended to a between subject variability names its omega element,
#' ie `om.eta.cl` is the omega element of `eta.cl`.
#'
#' @param nm character vector of names
#' @return the names with `om.` removed, or NULL when they are not all
#'   `om.` prefixed
#' @noRd
#' @author Matthew L. Fidler
.lotriStripOm <- function(nm) {
  if (length(nm) > 0L && all(grepl("^om[.].", nm))) {
    return(sub("^om[.]", "", nm))
  }
  NULL
}

#' Number of matrix elements a `~` right hand side would parse to
#'
#' `om.`-prefixed lines are ambiguous: `om.eta.cl ~ c(...)` is either the
#' next row of the joint omega prior block's own running triangular
#' build up, or it is simply the next row of whatever ordinary matrix
#' block is open in the main environment (an `om.`-prefixed name is a
#' perfectly ordinary matrix parameter name, e.g. the omega element of a
#' combined theta+omega covariance matrix written by
#' `lotriAsExpression()`).  Only the element count -- matched against
#' each block's own running count -- can tell the two apart.
#'
#' @param x right hand side language object of a `~` line
#' @return integer element count, or `NA_integer_` when it cannot be
#'   determined without triggering a downstream parse error
#' @noRd
#' @author Matthew L. Fidler
.lotriRhsLen <- function(x) {
  .n <- try(length(.lotriParseMat(x, env=new.env(parent=emptyenv()), noMat=TRUE)[[1]]),
            silent=TRUE)
  if (inherits(.n, "try-error")) return(NA_integer_)
  .n
}

#' Is this an `om.eta ~ variance` normal prior line?
#'
#' @param x language object to test
#' @param env parsing environment, which carries the running element
#'   count of any joint omega prior block already in progress
#' @return TRUE when every name on the left is `om.` prefixed and the
#'   right hand side is a plausible next row of the joint omega prior
#'   block (rather than of an ordinary matrix block already open in
#'   `env`)
#' @noRd
#' @author Matthew L. Fidler
.lotriIsOmegaPriorLine <- function(x, env) {
  if (!(is.call(x) && length(x) == 3L && identical(x[[1]], quote(`~`)))) {
    return(FALSE)
  }
  if (is.call(x[[3]]) && identical(x[[3]][[1]], quote(`|`))) return(FALSE)
  .nm <- .lotriTildeLhsNames(x[[2]])
  if (is.null(.nm) || is.null(.lotriStripOm(.nm))) return(FALSE)
  ## a `+`-summed left hand side always declares a whole new prior block
  ## at once (the same way ordinary matrix syntax does), never a
  ## continuation of one already open in the main environment, so it is
  ## unambiguously the joint prior shorthand
  if (length(.nm) > 1L) return(TRUE)
  .len <- .lotriRhsLen(x[[3]])
  ## a single value always starts a fresh block, for a prior row just as
  ## much as for an ordinary matrix row, so it stays ambiguous in favor
  ## of the prior (the long-standing behavior for every `om.x ~ value`
  ## line)
  if (is.na(.len) || .len == 1L) return(TRUE)
  ## the shorthand always puts a prior on an eta that already exists
  ## elsewhere in the matrix -- an `om.` line never creates one (see
  ## "om. names must match a between subject variability" below).  So
  ## when the stripped target is not a name declared *anywhere* in the
  ## block (checked against a full up-front scan, not just what has
  ## been parsed so far -- prior lines, like `prior()` lines, are not
  ## required to follow their target's declaration), this cannot be a
  ## valid prior target regardless of how its row count compares to
  ## anything, and is unambiguously an ordinary matrix row that simply
  ## happens to be `om.`-named (as `lotriAsExpression()` writes for the
  ## omega element of a combined theta+omega covariance matrix, see #53)
  if (!(.lotriStripOm(.nm) %in% env$etaLhsNames)) return(FALSE)
  ## otherwise the target is a real eta, so the row is a plausible
  ## prior row too; only the element count -- matched against each
  ## block's own running count -- can settle it.  Prefer the joint
  ## omega prior block's own running count (this also covers a fresh
  ## `om.x ~ c(...)` line that grows an already-started chain by more
  ## than one element at once), and fall through to the ordinary matrix
  ## row handling only when it does not fit there but does fit the
  ## block already open in the main environment.  A main `lastN` of
  ## exactly 1 is excluded -- every single scalar row leaves it at 1
  ## whether or not anything is actually still open for continuation
  ## (a `+`-declared block does not reset it either), so it is not a
  ## reliable signal on its own; two or more is only reachable through
  ## an actual multi-row continuation.  This does mean a genuine 2-row
  ## matrix whose *second* row is the first `om.`-prefixed line of the
  ## block (`lastN == 1` at that point) still resolves in favor of the
  ## prior when the stripped target also happens to be a real,
  ## separately declared eta -- the same pre-existing, unresolvable
  ## ambiguity as any other single-value `om.` line (see above); this
  ## was already true before this row ever reached this branch
  .priorLastN <- if (is.null(env$omegaPriorEnv)) 0L else env$omegaPriorEnv$lastN
  .matchesPrior <- .len == .priorLastN + 1L
  .matchesMain <- env$lastN >= 2L && .len == env$lastN + 1L
  if (.matchesPrior && .matchesMain) {
    ## genuinely ambiguous: this row would validly continue *both* the
    ## joint omega prior block and the matrix already open in the main
    ## environment.  Rather than silently guessing (and possibly
    ## dropping the row that was not chosen), fail loudly -- `prior()`
    ## names its target explicitly, so it is never ambiguous this way
    stop("'", .deparse1(x), # nolint
         "' is ambiguous: it could continue either the joint omega ",
         "prior block or the matrix already open here; write it as ",
         "'prior(", .lotriStripOm(.nm), ") ~ ...' to put a prior on it unambiguously",
         call.=FALSE)
  }
  if (.matchesPrior) return(TRUE)
  !.matchesMain
}

#' Handle the `om.eta ~ variance` normal prior shorthand
#'
#' Kept in its own environment so that an omega prior line never joins a
#' population estimate prior block, or an eta matrix, by accident.
#'
#' @param x language object of the prior line
#' @param env parsing environment
#' @return nothing, called for the side effect on `env$omegaPriorEnv`
#' @noRd
#' @author Matthew L. Fidler
.fCallOmegaPrior <- function(x, env) {
  if (is.null(env$omegaPriorEnv)) {
    env$omegaPriorEnv <- .lotriNewPriorEnv()
  }
  .fCallTilde(x, env$omegaPriorEnv)
  invisible()
}

#' Is this a joint population estimate + `om.` omega element prior line?
#'
#' A NONMEM `TNPRI` model puts the thetas and the omega elements in one
#' variance matrix, so a prior block may name both, ie
#' `tcl + om.eta.cl ~ c(1, 0.02, 0.01)`.  Every name has to be either a
#' population estimate or an `om.` name, and there has to be at least one
#' of each -- a line that is entirely one kind is handled by
#' `.lotriIsThetaPriorLine()` or `.lotriIsOmegaPriorLine()`.
#'
#' @param x language object to test
#' @param env parsing environment, which carries the estimate names
#' @return TRUE when this is a mixed prior line
#' @noRd
#' @author Matthew L. Fidler
.lotriIsJointPriorLine <- function(x, env) {
  if (!(is.call(x) && length(x) == 3L && identical(x[[1]], quote(`~`)))) {
    return(FALSE)
  }
  if (is.null(env$thetaNames)) return(FALSE)
  ## a conditioned block (`| id`) is always a matrix, never a prior
  if (is.call(x[[3]]) && identical(x[[3]][[1]], quote(`|`))) return(FALSE)
  .nm <- .lotriTildeLhsNames(x[[2]])
  if (is.null(.nm) || length(.nm) < 2L) return(FALSE)
  .isOm <- grepl("^om[.].", .nm)
  .isTh <- .nm %in% env$thetaNames
  any(.isOm) && any(.isTh) && all(.isOm | .isTh)
}

#' Handle a joint population estimate + `om.` prior line
#'
#' Kept in its own environment for the same reason the other two are: a
#' joint block must not merge into either the estimate prior block or the
#' omega prior block.
#'
#' @param x language object of the prior line
#' @param env parsing environment
#' @return nothing, called for the side effect on `env$jointPriorEnv`
#' @noRd
#' @author Matthew L. Fidler
.fCallJointPrior <- function(x, env) {
  if (is.null(env$jointPriorEnv)) {
    env$jointPriorEnv <- .lotriNewPriorEnv()
  }
  .fCallTilde(x, env$jointPriorEnv)
  invisible()
}

#' A scratch environment for accumulating normal prior lines
#'
#' @return a new environment set up for `.fCallTilde()`
#' @noRd
#' @author Matthew L. Fidler
.lotriNewPriorEnv <- function() {
  .env <- new.env(parent = emptyenv())
  .env$isCov <- FALSE
  .env$fun <- NULL
  .env$rcm <- FALSE
  .env$df <- NULL
  .env$lastN <- 0
  .env$eta1 <- 0L
  .env$cnd <- character()
  .env$names <- character(0)
  .env$labels <- character(0)
  .env
}

#' Is this a `theta ~ variance` normal prior line?
#'
#' A `~` whose left hand side names only *population estimates* cannot be
#' a matrix specification (the names would collide with the estimates), so
#' it is taken as the shorthand for a normal prior on those estimates.
#'
#' @param x language object to test
#' @param env parsing environment, which carries the estimate names
#' @return TRUE when this is the normal prior shorthand
#' @noRd
#' @author Matthew L. Fidler
.lotriIsThetaPriorLine <- function(x, env) {
  if (!(is.call(x) && length(x) == 3L && identical(x[[1]], quote(`~`)))) {
    return(FALSE)
  }
  if (is.null(env$thetaNames)) return(FALSE)
  ## a conditioned block (`| id`) is always a matrix, never a prior
  if (is.call(x[[3]]) && identical(x[[3]][[1]], quote(`|`))) return(FALSE)
  .nm <- .lotriTildeLhsNames(x[[2]])
  !is.null(.nm) && all(.nm %in% env$thetaNames)
}

#' Handle the `theta ~ variance` normal prior shorthand
#'
#' `tka <- 0.45; tka ~ 1` is a `dnorm(0.45, 1)` prior on `tka`, and
#' `tcl + tv ~ c(1, 0, 1)` is a multivariate normal prior on `tcl` and
#' `tv` centered on their estimates.  The number on the `~` is the
#' variance; the mean is the estimate given with `<-`.
#'
#' @param x language object of the prior line
#' @param env parsing environment
#' @return nothing, called for the side effect on `env$priors`
#' @noRd
#' @author Matthew L. Fidler
.fCallThetaPrior <- function(x, env) {
  if (is.null(env$thetaPriorEnv)) {
    env$thetaPriorEnv <- .lotriNewPriorEnv()
  }
  ## fed through the ordinary matrix parser so that every matrix
  ## spelling works here too: the plus form, the per row line form
  ## (`tcl ~ 1; tv ~ c(0.01, 1)`), and `sd()`/`cor()`/`chol()`
  .fCallTilde(x, env$thetaPriorEnv)
  invisible()
}

#' Prior means for a set of names, defaulting to zero
#'
#' A normal prior written with the shorthand is centered on what the
#' model already says the parameter is, so the mean comes from the
#' estimates rather than being fixed at zero.  A name with no estimate
#' to look up (which cannot happen for the shorthand, since the line is
#' only recognized when every name is an estimate) falls back to zero.
#'
#' @param nms character vector of parameter names
#' @param means named numeric vector of means, or `NULL` for all zero
#' @return numeric vector the same length as `nms`
#' @noRd
#' @author Matthew L. Fidler
.lotriPriorMeans <- function(nms, means) {
  if (is.null(means)) return(rep(0.0, length(nms)))
  .m <- unname(means[nms])
  .m[is.na(.m)] <- 0.0
  .m
}

#' Diagonal of the eta matrix being accumulated, keyed by `om.` name
#'
#' A joint TNPRI block puts a prior on the omega *elements*, so those
#' entries are centered on the omega values the model already gives, the
#' same way a theta entry is centered on its estimate.  Read straight off
#' the accumulated triplets rather than through
#' `.lotriGetMatrixFromEnv()`, which mutates the environment.
#'
#' @param env parsing environment
#' @return named numeric vector keyed by `om.<eta>`, or `NULL`
#' @noRd
#' @author Matthew L. Fidler
.lotriOmegaDiagMeans <- function(env) {
  .one <- function(e) {
    if (!is.environment(e) || is.null(e$df) || length(e$df$i) == 0L) {
      return(NULL)
    }
    ## every matrix spec carries its diagonal, so this cannot come back
    ## empty through the DSL
    .w <- which(e$df$i == e$df$j)
    setNames(as.double(e$df$x[.w]), paste0("om.", e$names[e$df$i[.w]]))
  }
  ## a conditioned model keeps one sub environment per level, so an `om.`
  ## name often belongs to a level rather than to the unconditioned part
  .ret <- .one(env)
  for (.c in env$cnd) {
    .ret <- c(.ret, .one(env[[.c]]))
  }
  if (length(.ret) == 0L) return(NULL)
  .ret
}

#' Turn the accumulated `theta ~ variance` lines into priors
#'
#' The lines are collected into one matrix so that the per row line form
#' builds up a block the same way it does for etas.  The matrix is then
#' split into its blocks: a 1x1 block is a univariate normal prior and a
#' larger block is a multivariate normal prior.  Both are centered on the
#' estimates, so `tka <- 0.45; tka ~ 1` is `dnorm(0.45, 1)`.
#'
#' @param env parsing environment
#' @param which name of the scratch environment holding the lines
#' @param means named numeric vector of prior means, or `NULL` for zero
#' @return nothing, called for the side effect on `env$priors`
#' @noRd
#' @author Matthew L. Fidler
.lotriThetaPriorsFromEnv <- function(env, which="thetaPriorEnv", means=NULL) {
  if (is.null(env[[which]])) return(invisible())
  .mat <- .lotriGetMatrixFromEnv(env[[which]])
  if (dim(.mat)[1] == 0L) return(invisible())
  attr(.mat, "lotriFix") <- NULL
  attr(.mat, "lotriUnfix") <- NULL
  attr(.mat, "lotriLabels") <- NULL
  class(.mat) <- NULL
  for (.blk in lotriMatInv(.mat)) { # nolint
    .nm <- dimnames(.blk)[[1]]
    env$priors <- c(env$priors,
                    list(list(names=.nm,
                              info=.lotriPriorNormalize(
                                str2lang(.lotriNormalPriorText(.blk, means))))))
  }
  invisible()
}

#' Turn the held `prior(name) ~ variance` lines into priors
#'
#' Run once the walk is over, so the means the shorthand centers on are
#' known.  An uncorrelated group becomes independent normal priors, the
#' same as the bare shorthand gives.
#'
#' @param env parsing environment
#' @param means named vector of prior means
#' @return nothing, called for the side effect on `env$priors`
#' @noRd
#' @author Matthew L. Fidler
#' Make an omega mean reachable under either spelling
#'
#' `prior(eta.cl)` and `prior(om.eta.cl)` name the same thing, so a
#' shorthand written either way has to center on the same omega value.
#'
#' @param means named vector of prior means, or NULL
#' @return `means` with each `om.x` also available as `x`
#' @noRd
#' @author Matthew L. Fidler
.lotriPriorMeanAlias <- function(means) {
  if (is.null(means)) return(means)
  .om <- grep("^om[.].", names(means), value=TRUE)
  if (length(.om) == 0L) return(means)
  .alias <- means[.om]
  names(.alias) <- sub("^om[.]", "", .om)
  .alias <- .alias[!(names(.alias) %in% names(means))]
  c(means, .alias)
}

#' The normal prior a variance specification stands for
#'
#' Shared by the two ways of writing it: a bare `tka ~ 0.1` line, and a
#' `prior(tka) ~ 0.1` which is the only form available when piping.
#'
#' @param blk one covariance block, with dimnames
#' @param means named vector of prior means, or NULL for all zero
#' @return the prior as text, ie `"dnorm(0, 1)"`
#' @noRd
#' @author Matthew L. Fidler
.lotriNormalPriorText <- function(blk, means=NULL) {
  .nm <- dimnames(blk)[[1]]
  ## unnamed so that the deparsed prior is `dnorm(0, 1)` and not
  ## `dnorm(0, c(tcl = 1))`
  .d <- unname(diag(blk))
  .w <- which(.d == 0)
  if (length(.w) > 0L) {
    stop("a normal prior on '", paste(.nm[.w], collapse="', '"),
         "' cannot have zero variance; did you mean 'fix()'?", call.=FALSE)
  }
  .w <- which(.d < 0)
  if (length(.w) > 0L) {
    stop("a normal prior on '", paste(.nm[.w], collapse="', '"),
         "' cannot have a negative variance", call.=FALSE)
  }
  .mu <- .lotriPriorMeans(.nm, means)
  if (length(.nm) == 1L) {
    return(paste0("dnorm(", .deparse1(.mu), ", ", # nolint
                  .deparse1(sqrt(.d[1])), ")")) # nolint
  }
  ## an all zero mean vector stays the scalar `0` it has always been
  ## deparsed as, so only a real mean widens the text
  .muTxt <- if (all(.mu == 0)) "0" else .deparse1(.mu) # nolint
  ## the covariance is kept as the lotri expression that built it, which
  ## is valid R and round trips exactly
  paste0("multiNormal(", .muTxt, ", lotri(",
         .deparse1(.lotriGetEtaMatEltPlusForm(blk)[[1]]), "))") # nolint
}

#' Do these names make up exactly one covariance block of `mat`?
#'
#' @param mat matrix to check
#' @param nms character vector of names
#' @return boolean
#' @noRd
#' @author Matthew L. Fidler
.lotriNamesAreBlock <- function(mat, nms) {
  .dn <- dimnames(mat)[[1]]
  .i <- match(nms, .dn)
  if (anyNA(.i)) return(FALSE)
  .i <- sort(.i)
  ## the names have to be *exactly* one block; a set of unconnected
  ## diagonal elements is a set of 1x1 blocks, not a single block
  identical(as.integer(.lotriBlockIndexes(mat, .i[1])), as.integer(.i))
}

#' Do these two names name ONE covariance (off-diagonal) element -- ie are
#' they two DIFFERENT members of the SAME connected block, not necessarily
#' the whole block?
#'
#' The relaxed sibling of `.lotriNamesAreBlock()`: a marginal prior on one
#' covariance cell only needs its two names to covary with each other, not
#' to exhaust the whole block the way a joint (`invWishart()`/
#' `multiNormal()`) block prior does.
#'
#' @param mat matrix to check
#' @param nms character vector of exactly two names
#' @return boolean
#' @noRd
#' @author Matthew L. Fidler
.lotriNamesAreCovPair <- function(mat, nms) {
  if (length(nms) != 2L) return(FALSE)
  .dn <- dimnames(mat)[[1]]
  .i <- match(nms, .dn)
  if (anyNA(.i) || .i[1] == .i[2]) return(FALSE)
  .i[2] %in% .lotriBlockIndexes(mat, .i[1])
}

#' Split a `"lotriOffDiagPriors"` key back into its two names
#'
#' The key is `"(name_i,name_j)"` -- the exact string
#' `.as.data.frame.lotriFix.mat()` builds for a covariance element's `name`
#' column. A bare R symbol can never contain `,`/`(`/`)`, so this split is
#' unambiguous.
#'
#' @param key a single `"(name_i,name_j)"` string
#' @return character(2): `c(name_i, name_j)`
#' @noRd
#' @author Matthew L. Fidler
.lotriCovPriorKeyNames <- function(key) {
  strsplit(substring(key, 2L, nchar(key) - 1L), ",", fixed=TRUE)[[1]]
}

#' Indexes of the covariance block containing element `i`
#'
#' @param mat matrix to examine
#' @param i index of an element in the block
#' @return integer vector of the indexes making up the block
#' @noRd
#' @author Matthew L. Fidler
.lotriBlockIndexes <- function(mat, i) {
  .n <- dim(mat)[1]
  .lo <- i
  .hi <- i
  repeat {
    .changed <- FALSE
    if (.lo > 1L && any(mat[seq(.lo, .hi), .lo - 1L] != 0)) {
      .lo <- .lo - 1L
      .changed <- TRUE
    }
    if (.hi < .n && any(mat[seq(.lo, .hi), .hi + 1L] != 0)) {
      .hi <- .hi + 1L
      .changed <- TRUE
    }
    if (!.changed) break
  }
  seq(.lo, .hi)
}

#' Fixed status of a set of diagonal (eta) names within a matrix
#'
#' @param m matrix, possibly carrying a `lotriFix` attribute (a same
#'   shaped logical matrix marking which elements are fixed)
#' @param names character vector of diagonal names to look up
#' @return logical vector, the same length and order as `names`
#' @noRd
#' @author Matthew L. Fidler
.lotriMatFixedDiag <- function(m, names) {
  .fx <- attr(m, "lotriFix")
  if (is.null(.fx)) return(rep(FALSE, length(names)))
  .dn <- dimnames(m)[[1]]
  vapply(names, function(.n) {
    .i <- match(.n, .dn)
    if (is.na(.i)) return(FALSE)
    isTRUE(.fx[.i, .i])
  }, logical(1), USE.NAMES=FALSE)
}

#' Whether any covariance (off-diagonal) entry among a set of names is fixed
#'
#' Diagonal (variance) fix status is checked name-by-name by
#' `.lotriMatFixedDiag()`; a block prior like `lkjCorr()`/`invWishart()`
#' also models the covariances *between* the block's members, so it has
#' to be refused when one of those is fixed even though every variance
#' in the block is free.
#'
#' @param m matrix, possibly carrying a `lotriFix` attribute
#' @param names character vector of the block's names
#' @return TRUE if any off-diagonal entry among `names` is fixed
#' @noRd
#' @author Matthew L. Fidler
.lotriMatFixedCov <- function(m, names) {
  if (length(names) < 2L) return(FALSE)
  .fx <- attr(m, "lotriFix")
  if (is.null(.fx)) return(FALSE)
  .dn <- dimnames(m)[[1]]
  .idx <- match(names, .dn)
  if (any(is.na(.idx))) return(FALSE)
  .sub <- .fx[.idx, .idx, drop=FALSE]
  diag(.sub) <- FALSE
  any(.sub)
}

#' Whether a block has nothing left in it that could take a prior
#'
#' Used only to decide whether the implicit `~invWishart(4)` shorthand
#' quietly skips a block instead of applying to it: every variance *and*
#' every covariance in the block has to be fixed, not just the
#' diagonal, or the block still has a free entry that the shorthand
#' would otherwise silently leave without a prior.
#'
#' @param m matrix, possibly carrying a `lotriFix` attribute
#' @param names character vector of the block's names
#' @return TRUE when every entry among `names` (diagonal and
#'   off-diagonal alike) is fixed
#' @noRd
#' @author Matthew L. Fidler
.lotriMatEntirelyFixed <- function(m, names) {
  .fx <- attr(m, "lotriFix")
  if (is.null(.fx)) return(FALSE)
  .dn <- dimnames(m)[[1]]
  .idx <- match(names, .dn)
  if (any(is.na(.idx))) return(FALSE)
  all(.fx[.idx, .idx, drop=FALSE])
}

#' Resolve a joint population estimate + `om.` omega element prior
#'
#' The block spans two places -- the estimate table and the omega matrix
#' -- so it is stored once, on the first name of the block, rather than
#' being split.  The covariance keeps every name (`om.` prefix included)
#' so a consumer can recover which entries are omega elements.
#'
#' @param nm character vector of the block's names, in order
#' @param isOm logical, which of `nm` are `om.` prefixed
#' @param info normalized prior
#' @param est `lotriEst` data frame
#' @param mats list of matrices making up the result
#' @param pri list of per matrix prior character vectors
#' @return list with the amended `est` and `pri`
#' @noRd
#' @author Matthew L. Fidler
.lotriResolveJointPrior <- function(nm, isOm, info, est, mats, pri) {
  ## every `om.` name has to be a real between subject variability; it
  ## never quietly creates one
  .eta <- sub("^om[.]", "", nm[isOm])
  .at <- NA_integer_
  for (.k in seq_along(mats)) {
    .m <- mats[[.k]]
    .dn <- if (is.matrix(.m)) dimnames(.m)[[1]] else NULL
    ## a multi level model has one matrix per level, so the eta is often
    ## not in the first one
    if (is.null(.dn) || !all(.eta %in% .dn)) next
    .at <- .k
    break
  }
  if (is.na(.at)) {
    stop("prior given for unknown omega element(s): '",
         paste(nm[isOm], collapse="', '"), "'", call.=FALSE)
  }
  .w <- match(nm[!isOm], est$name)
  .lotriPriorCheckTarget(info, nm, est$lower[.w], est$upper[.w],
                         isBlock=FALSE, inMatrix=FALSE)
  .lotriPriorCheckNotFixed(nm[!isOm], est$fix[.w])
  .lotriPriorCheckNotFixed(nm[isOm], .lotriMatFixedDiag(mats[[.at]], .eta))
  .lotriPriorCheckNotFixedCov(nm[isOm], .lotriMatFixedCov(mats[[.at]], .eta))
  ## stored on the first name of the block, wherever that name lives
  if (isOm[1]) {
    .dn <- dimnames(mats[[.at]])[[1]]
    .i <- match(sub("^om[.]", "", nm[1]), .dn)
    if (!is.na(pri[[.at]][.i])) {
      stop("more than one prior given for '", nm[1], "'", call.=FALSE)
    }
    pri[[.at]][.i] <- info$text
  } else {
    .i <- match(nm[1], est$name)
    if (!is.na(est$prior[.i])) {
      stop("more than one prior given for '", nm[1], "'", call.=FALSE)
    }
    est$prior[.i] <- info$text
  }
  list(est=est, pri=pri)
}

#' Resolve the collected priors against the estimates and matrices
#'
#' Priors are matched by *name* (never by position) so that they are
#' unaffected by any `rcm` re-ordering of the matrix.  Priors on
#' population estimates become the `prior` column of the `lotriEst`
#' data frame; priors on etas (and on covariance blocks) become the
#' `lotriPriors` attribute of the matrix they belong to, stored on the
#' first diagonal element of the block.
#'
#' @param ret matrix or list of matrices
#' @param est `lotriEst` data frame (may be NULL)
#' @param priors list collected by `.fCallPrior()`
#' @return list with the amended `ret` and `est`
#' @noRd
#' @author Matthew L. Fidler
.lotriResolvePriors <- function(ret, est, priors, wholePriors=NULL) {
  if (length(priors) == 0L && length(wholePriors) == 0L) {
    return(list(ret=ret, est=est))
  }
  .isList <- !is.matrix(ret) && (inherits(ret, "list") || inherits(ret, "lotri"))
  .mats <- if (.isList) as.list(ret) else list(ret)
  ## `~invWishart(4)` names no block, so expand it to one entry per block
  ## and let the ordinary resolution below validate each of them
  if (length(wholePriors) > 0L) {
    .expand <- list()
    for (.wp in wholePriors) {
      for (.k in seq_along(.mats)) {
        .m <- .mats[[.k]]
        if (!is.matrix(.m) || dim(.m)[1] == 0L) next
        .dn <- dimnames(.m)[[1]]
        .i <- 1L
        while (.i <= length(.dn)) {
          .idx <- .lotriBlockIndexes(.m, .i)
          ## a block that is entirely fixed is already a constant; the
          ## implicit shorthand applies to every free block, so it
          ## quietly skips one instead of erroring the way an explicit
          ## `prior(om.eta) ~ ...` on a fixed element does below
          if (!.lotriMatEntirelyFixed(.m, .dn[.idx])) {
            .expand[[length(.expand) + 1L]] <- list(names=.dn[.idx], info=.wp)
          }
          .i <- max(.idx) + 1L
        }
      }
    }
    if (length(.expand) == 0L) {
      stop("'~", wholePriors[[1]]$text,
           "' was given but the model has no omega to apply it to",
           call.=FALSE)
    }
    priors <- c(.expand, priors)
  }
  .pri <- lapply(.mats, function(m) {
    if (!is.matrix(m)) return(character(0))
    rep(NA_character_, dim(m)[1])
  })
  ## off-diagonal (covariance) priors have no diagonal position to key on,
  ## so they get their own per-matrix named vector, keyed by the SAME
  ## "(name_i,name_j)" string (smaller-matrix-position name first) that
  ## `.as.data.frame.lotriFix.mat()` independently builds for that cell --
  ## reusing that string, rather than inventing a new key format, is what
  ## lets the two sides find each other with no extra bookkeeping.
  .priOff <- vector("list", length(.mats))
  .seen <- character(0)
  .jointOnOmega <- FALSE
  for (.p in priors) {
    .nm <- .p$names
    .info <- .p$info
    .key <- paste(sort(.nm), collapse=",")
    if (.key %in% .seen) {
      stop("more than one prior given for '", paste(.nm, collapse=", "), "'",
           call.=FALSE)
    }
    .seen <- c(.seen, .key)
    ## a joint theta + `om.` block: one multivariate normal spanning the
    ## estimates and the omega elements, which is what a NONMEM TNPRI
    ## variance matrix is.  It is stored once, on the first name of the
    ## block, and the covariance keeps every name so a consumer can split
    ## it back apart.
    .isOm <- grepl("^om[.].", .nm)
    if (!is.null(est) && any(.isOm) && any(.nm %in% est$name) &&
          all(.isOm | .nm %in% est$name)) {
      .res <- .lotriResolveJointPrior(.nm, .isOm, .info, est, .mats, .pri)
      est <- .res$est
      .pri <- .res$pri
      .jointOnOmega <- TRUE
      next
    }
    if (!is.null(est) && all(.nm %in% est$name)) {
      .w <- match(.nm, est$name)
      .lotriPriorCheckTarget(.info, .nm, est$lower[.w], est$upper[.w],
                             isBlock=FALSE, inMatrix=FALSE)
      .lotriPriorCheckNotFixed(.nm, est$fix[.w])
      if (any(!is.na(est$prior[.w]))) {
        stop("more than one prior given for '",
             paste(.nm[!is.na(est$prior[.w])], collapse="', '"), "'", call.=FALSE)
      }
      est$prior[.w] <- .info$text
      next
    }
    .found <- FALSE
    ## `om.eta.cl` names the omega element of `eta.cl`
    .om <- .lotriStripOm(.nm)
    for (.k in seq_along(.mats)) {
      .m <- .mats[[.k]]
      if (!is.matrix(.m)) next
      .dn <- dimnames(.m)[[1]]
      if (is.null(.dn)) next
      if (!all(.nm %in% .dn)) {
        if (is.null(.om) || !all(.om %in% .dn)) next
        .nm <- .om
      }
      .isBlock <- length(.nm) > 1L
      .isCovPair <- .isBlock && length(.nm) == 2L && .info$kind == "univariate" &&
        .lotriNamesAreCovPair(.m, .nm)
      if (.isBlock && !.isCovPair && !.lotriNamesAreBlock(.m, .nm)) {
        stop("'", paste(.nm, collapse=", "),
             "' is not a single covariance block, so it cannot share a prior",
             call.=FALSE)
      }
      .lotriPriorCheckTarget(.info, .nm, isBlock=.isBlock, inMatrix=TRUE,
                             isCovPair=.isCovPair)
      if (.isCovPair) {
        ## a covariance-pair prior targets ONLY the one off-diagonal cell,
        ## not either name's own variance, so only the covariance-fixed
        ## check applies here -- a fixed variance on eta.cl/eta.v does not
        ## block a prior on the covariance between them
        .lotriPriorCheckNotFixedCov(.nm, .lotriMatFixedCov(.m, .nm))
        .i <- sort(match(.nm, .dn))
        .key <- paste0("(", .dn[.i[1]], ",", .dn[.i[2]], ")")
        if (.key %in% names(.priOff[[.k]])) {
          stop("more than one prior given for '", paste(.nm, collapse=", "), "'",
               call.=FALSE)
        }
        .priOff[[.k]][.key] <- .info$text
      } else {
        .lotriPriorCheckNotFixed(.nm, .lotriMatFixedDiag(.m, .nm))
        .lotriPriorCheckNotFixedCov(.nm, .lotriMatFixedCov(.m, .nm))
        .at <- min(match(.nm, .dn))
        if (!is.na(.pri[[.k]][.at])) {
          stop("more than one prior given for '", paste(.nm, collapse=", "), "'",
               call.=FALSE)
        }
        .pri[[.k]][.at] <- .info$text
      }
      .found <- TRUE
      break
    }
    if (!.found) {
      stop("prior given for unknown parameter(s): '",
           paste(.nm, collapse="', '"), "'", call.=FALSE)
    }
  }
  ## degrees of freedom on an omega (a NONMEM NWPRI) and a normal prior
  ## on the omega values (a NONMEM TNPRI) are alternative ways of saying
  ## the same thing, so a model cannot carry both
  .fam <- unlist(lapply(seq_along(.pri), function(.k) {
    c(.lotriPriorFamily(.pri[[.k]]), .lotriPriorFamily(.priOff[[.k]]))
  }))
  if (any(.fam == "wishart", na.rm=TRUE) &&
        (any(.fam == "normal", na.rm=TRUE) || .jointOnOmega)) {
    stop("a model cannot have both degrees of freedom (ie 'invWishart()') ",
         "and a normal prior (ie 'om.eta ~ 0.1') on its omegas; these are ",
         "alternatives, not additions", call.=FALSE)
  }
  ## a marginal prior on one covariance cell and a whole-block prior
  ## (`invWishart()`/`multiNormal()`) both constrain that same cell, so a
  ## block cannot carry both.  This is NOT caught by the family check above:
  ## `multiNormal()` is itself family "normal" (indistinguishable by family
  ## alone from a marginal `dnorm()`), and a marginal `dcauchy()` is family
  ## "other" (neither "wishart" nor "normal") -- so overlap has to be
  ## detected by direct block membership instead.
  for (.k in seq_along(.mats)) {
    if (length(.priOff[[.k]]) == 0L || all(is.na(.pri[[.k]]))) next
    ## `.priOff[[.k]]` is only ever populated inside the per-prior loop
    ## above, which itself only reaches that assignment after confirming
    ## `.mats[[.k]]` is a matrix and its key's names resolve against that
    ## SAME matrix's dimnames -- so both are guaranteed here, not merely
    ## likely, and re-checking would be untestable dead code
    .m <- .mats[[.k]]
    .dn <- dimnames(.m)[[1]]
    for (.key in names(.priOff[[.k]])) {
      .nm2 <- .lotriCovPriorKeyNames(.key)
      .i <- match(.nm2[1], .dn)
      .blk <- .lotriBlockIndexes(.m, .i)
      if (any(!is.na(.pri[[.k]][.blk]))) {
        stop("'", paste(.nm2, collapse=", "), "' already has a whole-block ",
             "prior on its covariance block, so it cannot also carry a ",
             "marginal prior on one of its cells", call.=FALSE)
      }
    }
  }
  for (.k in seq_along(.mats)) {
    if (all(is.na(.pri[[.k]])) && length(.priOff[[.k]]) == 0L) next
    .m <- .mats[[.k]]
    if (!all(is.na(.pri[[.k]]))) attr(.m, "lotriPriors") <- .pri[[.k]]
    if (length(.priOff[[.k]]) > 0L) attr(.m, "lotriOffDiagPriors") <- .priOff[[.k]]
    if (!inherits(.m, "lotriFix")) {
      class(.m) <- c("lotriFix", class(.m))
    }
    .mats[[.k]] <- .m
  }
  if (.isList) {
    .attr <- attributes(ret)
    ret <- .mats
    attributes(ret) <- .attr
    ## the list itself has to be a `lotriFix` as well, or `as.expression()`
    ## and `print()` dispatch to the default methods and the priors are
    ## never shown
    if ((any(vapply(.pri, function(p) any(!is.na(p)), logical(1))) ||
           any(vapply(.priOff, length, integer(1)) > 0L)) &&
          !inherits(ret, "lotriFix")) {
      class(ret) <- c("lotriFix", class(ret))
    }
  } else {
    ret <- .mats[[1]]
  }
  list(ret=ret, est=est)
}

#' This handles the `~` operator in the lotri DSL.
#'
#'
#' @param x expression
#' @param env parsing environment
#' @return nothing, called for side effects to env
#' @noRd
#' @author Matthew L. Fidler
.fCall <- function(x, env) {
  if (.lotriIsSameLine(x)) {
    ## Checked before every prior branch: `.lotriIsThetaPriorLine()` would
    ## otherwise claim `tka ~ same()` whenever `tka` is a population
    ## estimate and route it into `.fCallTilde()` with a useless message.
    ## `lastTilde` is set so a following `label()` attaches to this line.
    .lotriEnv$lastTilde <- TRUE
    .fCallSame(x, env)
  } else if (.lotriIsPriorLine(x)) {
    ## Note this is checked *before* the `~` branch below so that
    ## `.lotriEnv$lastTilde` is not changed; otherwise a `label()`
    ## following a prior would be applied to the last matrix row.
    .fCallPrior(x, env)
  } else if (.lotriIsWholeOmegaPriorLine(x)) {
    ## `~invWishart(4)` gives every omega block the same prior
    .fCallWholeOmegaPrior(x, env)
  } else if (.lotriIsJointPriorLine(x, env)) {
    ## `tcl + om.eta.cl ~ c(...)` is one multivariate normal over a theta
    ## and an omega element, which is the joint variance a NONMEM TNPRI
    ## model uses; checked before the two single kind branches, which each
    ## require every name to be of their own kind
    .fCallJointPrior(x, env)
  } else if (.lotriIsOmegaPriorLine(x, env)) {
    ## `om.eta.cl ~ 0.01` is a normal prior on the omega element of
    ## `eta.cl`, which is what a NONMEM TNPRI model needs
    .fCallOmegaPrior(x, env)
  } else if (.lotriIsThetaPriorLine(x, env)) {
    ## `tka ~ 1` where `tka` is a population estimate is a normal prior,
    ## not an eta; checked before `lastTilde` is set for the same reason
    ## as the `prior()` branch above
    .fCallThetaPrior(x, env)
  } else if (identical(x[[1]], quote(`~`))) {
    .lotriEnv$lastTilde <- TRUE
    .fCallTilde(x, env)
  } else if (identical(x[[1]], quote(`{`))) {
    .x <- x[-1]
    for (.i in seq_along(.x)) {
      .curLine <- try(.f(.x[[.i]], env=env), silent=TRUE)
      if (inherits(.curLine, "try-error")) {
        env$.hasErr <- TRUE
        env$.err[[.i]] <- paste(c(env$.err[[.i]], attr(.curLine, "condition")$message), collapse="\n")
      }
    }
  } else if (identical(x[[1]], quote(`quote`))) {
    lapply(x[[2]], .f, env = env)
  } else if (identical(x[[1]], quote(`matrix`))) {
    if (!is.null(env$matrix)) {
      stop("only one matrix can be in an expression")
    }
    env$matrix <- eval(x, envir=.lotriParentEnv)
  } else if (identical(x[[1]], quote(`=`)) ||
               identical(x[[1]], quote(`<-`))) {
    .lotriEnv$lastTilde <- FALSE
    ## these are handled in .parseThetaEst()
    .resetLastN(env, 0L)
  } else if (.lotriEnv$lastTilde &&
               identical(x[[1]], quote(`label`))) {
    # only the last tilde is labeled
    if (is.null(env$labels)) {
      if (exists("lastCnd", env) &&
            exists(env$lastCnd,env)) {
        .lab <- env[[env$lastCnd]]$labels
        env[[env$lastCnd]]$labels[length(.lab)] <- x[[2]]
      }
    } else {
      env$labels[length(env$labels)] <- x[[2]]
    }
  } else if (identical(x[[1]], quote(`label`)) ||
               identical(x[[1]], quote(`backTransform`))) {
    ## these are handled in .parseThetaEst()
  } else {
    stop("matrix expression should be 'name ~ c(lower-tri)'", call. = FALSE)
  }
}

#' DSL parsing function
#'
#' @param x Parsing tree
#' @param env environment to update
#' @return Nothing
#' @author Matthew Fidler
#' @noRd
.f <- function(x, env) {
  if (is.name(x)) {
    character()
  } else if (is.call(x)) {
    .fCall(x, env)
  } else {
    ## is.pairlist OR is.atomic OR unknown...
    stop("bad matrix specification", call. = FALSE)
  }
}
#' Parses condition
#'
#' @param cond Condition parsing tree
#' @param envir Environment to parse condition in.
#'
#' @return list with 2 elements: - First element is the name of the condition - Second element is extra information
#' @author Matthew Fidler
#' @noRd
.parseCondition <- function(cond, envir = parent.frame()) {
  if (length(cond) == 1) {
    .fullCnd <- as.character(cond)
    return(list(.fullCnd, NULL))
  }
  .fullCnd <- as.character(cond[[1]])
  if (regexpr("^[a-zA-Z][a-zA-Z0-9_.]*$", .fullCnd) == -1) {
    .cnd <- .deparse1(cond) # nolint
    stop("unsupported conditional statement: '",
         .deparse1(.cnd), # nolint
         "'",
         call. = FALSE)
  }
  .env <- list2env(as.list(envir), parent = globalenv())
  .env[[.fullCnd]] <- function(...) {
    list(...)
  }
  .prop <- eval(cond, envir = .env)
  list(.fullCnd, .prop)
}

.defaultProperties <- c(lower = -Inf, upper = Inf)

#' Amplify Default properties
#'
#' @param prop proprety list where `.defaultProperties` will be amplified
#' @param names names of matrix components to check against
#' @return Amplified property list
#' @author Matthew Fidler
#' @noRd
.amplifyDefault <- function(prop, names) {
  .nD <- names(.defaultProperties)
  .newProp <- prop
  for (.n in .nD) {
    if (any(.n == names(prop))) {
      .cur <- prop[[.n]]
      if (is.null(names(.cur))) {
        if (length(.cur) != 1) {
          stop(sprintf(
            gettext("name multiple limits for '%s': '%s=c(%s=%s,...)'"),
            .n, .n, names[1], .cur[1]
          ), call. = FALSE)
        } else {
          .newProp[[.n]] <- setNames(rep(.cur, length(names)), names)
          next
        }
      }
      .new <- setNames(rep(.defaultProperties[.n], length(names)), names)
      .bad <- NULL
      for (.n2 in names(.cur)) {
        if (is.na(.new[.n2])) {
          .bad <- c(.bad, .n2)
        } else {
          .new[.n2] <- .cur[.n2]
        }
      }
      if (length(.bad) > 0) {
        stop(sprintf(
          gettext("in '%s' argument/dimension mismatch: %s"),
          .n, paste(.bad, collapse = ", ")
        ), call. = FALSE)
      }
      .newProp[[.n]] <- .new
    }
  }
  .newProp
}
#' Amplifies final lotri list with defaults in .defaultProperties
#'
#' @param finalList Final List before return
#' @param prop current properties
#' @return lotri amplified with defaults for all parameters
#' @author Matthew Fidler
#' @noRd
.amplifyFinal <- function(finalList, prop) {
  for (.p in names(prop)) {
    .cur <- prop[[.p]]
    .dim <- dimnames(finalList[[.p]])[[1]]
    for (.d in names(.defaultProperties)) {
      if (any(names(.cur) == .d)) {
        .final <- setNames(rep(.defaultProperties[.d], length(.dim)), .dim)
        .curD <- .cur[[.d]]
        for (.c in names(.curD)) {
          .final[.c] <- .curD[.c]
        }
        .cur[[.d]] <- .final
      }
    }
    prop[[.p]] <- .cur
  }
  prop
}
#' Merge properties between two matrices
#'
#' @param prop Initial property list or character vector of names to
#'   apply default properties on...
#' @param id ID of the matrix with more properites
#' @param new new properites of the matrix
#' @return A merged property that will be used for lotri composite
#'   matrices
#' @author Matthew Fidler
#' @noRd
.mergeProp <- function(prop, id, new) {
  if (is.null(prop)) {
    .ret <- list()
    .ret[[id]] <- new
    return(.ret)
  }
  if (!inherits(prop, "list")) {
    for (.n in names(new)) {
      if (any(.n == names(.defaultProperties))) {
        new[[.n]] <- c(
          new[[.n]],
          setNames(rep(
            .defaultProperties[.n],
            length(prop)
          ), prop)
        )
      }
    }
    .ret <- list()
    .ret[[id]] <- new
    return(.ret)
  }
  .old <- prop[[id]]
  for (.n in names(.old)) {
    if (any(.n == names(.defaultProperties))) {
      ## These are fully completed before reaching the merging point
      .old[[.n]] <- c(new[[.n]], .old[[.n]])
      new <- new[names(new) != .n]
    } else if (any(.n == names(new))) {
      stop(sprintf(gettext("conflicting '%s' properties"), .n), call. = FALSE)
    }
  }
  for (.n in names(new)) {
    .old[[.n]] <- new[[.n]]
  }
  .ret <- prop
  .ret[[id]] <- .old
  .ret
}

#' Extract a matrix saved in the environment
#'
#' @param env Environment where matrix is saved
#' @param val value where the matrix is saved in
#' @return named matrix
#' @author Matthew Fidler
#' @noRd
.getMatrix <- function(env, val) {
  .Call(`_lotriLstToMat`, # nolint
        env[[val]], NULL, 1L, class(matrix(0)), PACKAGE = "lotri")
}

.lotriList <- function(x, ..., envir = parent.frame()) {
  omega <- lapply(x, lotri, envir = envir)
  if (inherits(omega, "list")) {
    .env <- new.env(parent = emptyenv())
    .env[["...cnd"]] <- NULL
    .env[["...empty"]] <- list()
    lapply(seq_along(omega), function(x) {
      .cur <- omega[[x]]
      .curName <- names(omega)[x]
      if (is.null(.curName)) {
        .curName <- ""
      }
      if (inherits(.cur, "matrix")) {
        if (.curName == "") {
          assign("...empty", c(.env[["...empty"]], list(.cur)), .env) # nolint
        } else {
          assign(.curName, c(.env[[.curName]], list(.cur)), .env)
          assign("...cnd", unique(c(.env[["...cnd"]], .curName)), .env) # nolint
        }
      } else if (inherits(.cur, "list") || inherits(.cur, "lotri")) {
        lapply(
          seq_along(.cur),
          function(y) {
            .cury <- .cur[[y]]
            .curName <- names(.cur)[y]
            if (.curName == "") {
              assign("...empty", c( # nolint
                .env[["...empty"]], # nolint
                list(.cury)
              ), .env)
            } else {
              assign(.curName, list(.cury), .env)
              assign("...cnd", unique(c( # nolint
                .env[["...cnd"]], # nolint
                .curName
              )), .env)
            }
          }
        )
      }
    })
    if (length(.env$...empty) > 0) {
      .omega <- .getMatrix(.env, "...empty")
    } else {
      .omega <- NULL
    }
    if (length(.env$...cnd) > 0) {
      .lst <- setNames(lapply(.env$...cnd, function(cnd) {
        .getMatrix(.env, cnd)
      }), .env$...cnd)
      if (!is.null(.omega)) {
        .lst <- c(list(.omega), .lst)
      }
      omega <- .lst
    } else {
      omega <- .omega
    }
  }
  omega
}

.lotriParentEnv <- NULL

#' Amplify the return with the fixed estimates (if present)
#'
#' @param ret return value to amplify with fixed estimates
#' @param df data frame of fixed estimates
#' @return Amplified return value with fixed estimates
#' @noRd
#' @author Matthew L. Fidler
.amplifyRetWithDfEst <- function(ret, df) {
  if (is.null(df)) return(ret)
  attr(ret, "lotriEst") <- df
  .allNames <- c(dimnames(ret)[[1]], df$name)
  .dup <- unique(.allNames[duplicated(.allNames)])
  if (length(.dup) > 0) {
    stop("duplicated parameter(s): '",paste(.dup, collapse="', '"), "'", sep="",
         call.=FALSE)
  }
  if ((inherits(ret, "matrix") || inherits(ret, "list") || inherits(ret, "lotri")) &&
        !inherits(ret, "lotriFix")) {
    class(ret) <- c("lotriFix", class(ret))
  }
  ret
}

#' This asserts the covariance values are zero when variances are zero
#'
#' @param ret matrix to consider
#' @param cnd level currently being examined
#' @return the negative indexes of the zero diagonals
#' @noRd
#' @author Matthew L. Fidler
.assertErrZeroDiag <- function(ret, cnd) {
  .cnd <- ""
  if (!is.null(cnd)) {
    .cnd <- paste0(", level ", cnd)
  }
  .zd <- integer(0)
  for (idx1 in seq_len(nrow(ret))) {
    .zeroDiag <- ret[idx1, idx1] == 0
    if (.zeroDiag) {
      .zd <- c(.zd, -idx1)
      .nonDiagidx <- setdiff(seq_len(ncol(ret)), idx1)
      for (idx2 in .nonDiagidx) {
        .badValue <- FALSE
        if (ret[idx1, idx2] != 0) {
          # already symmetric no need to check idx2, idx1
          .idxRow <- idx1
          .idxCol <- idx2
          .badValue <- TRUE
        }
        if (.badValue) {
          stop("if diagonals are zero, off-diagonals must be zero for covariance matrices (row ", # nolint
               .idxRow, ", column ", .idxCol, .cnd, ")",
               call.=FALSE)
        }
      }
    }
  }
  .zd
}
#'
#' Create the matrix from the lotri environment
#'
#' @param env lotri environment
#' @param cnd current condition
#' @return matrix
#' @noRd
#' @author Bill Denney & Matthew L. Fidler
.lotriGetMatrixFromEnv <- function(env, cnd=NULL, fun=NULL) {
  if (is.null(env$df)) {
    return(matrix(nrow=0, ncol=0))
  }
  if (length(env$df$i) == 0L) {
    return(matrix(nrow=0, ncol=0))
  }
  env$eta1 <- max(env$df$i)
  .ret <- diag(env$eta1)
  .n <- dim(.ret)[1]
  .retF <- matrix(FALSE, dim(.ret)[1], .n)
  .retU <- matrix(FALSE, dim(.ret)[1], .n)
  for (.i in seq_along(env$df$i)) {
    .ret[env$df$i[.i], env$df$j[.i]] <- env$df$x[.i]
    .retF[env$df$i[.i], env$df$j[.i]] <- env$df$fix[.i]
    .retU[env$df$i[.i], env$df$j[.i]] <- env$df$unfix[.i]
  }
  dimnames(.ret) <- list(env$names, env$names)
  dimnames(.retF) <- list(env$names, env$names)
  dimnames(.retU) <- list(env$names, env$names)
  .lotriSamePad(env)
  .hasSame <- any(env$sameOff != 0L)
  if (.hasSame && is.logical(env$rcm) && env$rcm) {
    ## the permutation would separate a block from the block it repeats,
    ## and `as.expression()` could then no longer re-emit `same()`
    stop("'rcm' cannot be used with 'same()'", call.=FALSE)
  }
  if (is.logical(env$rcm) && env$rcm && .n >= 1 &&
        !lotriIsBlockMat(.ret)) { # nolint
    .old <- env$names
    .ret <- rcm(.ret) # nolint
    env$names <- dimnames(.ret)[[1]]
    .retF <- .retF[env$names, env$names]
    .retU <- .retU[env$names, env$names]
    ## the labels are stored in parse order, so they have to follow the
    ## permutation too (otherwise they end up on the wrong parameter)
    if (!is.null(env$labels)) {
      env$labels <- env$labels[match(env$names, .old)]
    }
  }
  if (env$isCov) {
    .assertErrZeroDiag(.ret, cnd)
    if (.hasSame && is.function(fun)) {
      ## the correction is applied to the whole matrix and would move the
      ## copies away from the block they repeat, making `lotriSame` a lie
      stop("a 'cov' function cannot be used with 'same()'", call.=FALSE)
    }
    if (is.function(fun)) {
      .ret2 <- fun(.ret)
      if (!is.matrix(.ret2)) {
        stop("'cov' function must return a matrix", call.=FALSE)
      }
      if (!identical(dim(.ret2), dim(.ret))) {
        stop("'cov' function must return a matrix with the same dimensions", call.=FALSE)
      }
      .dn <- dimnames(.ret2)
      if (is.null(.dn) || is.null(.dn[[1]]) || is.null(.dn[[2]])) {
        dimnames(.ret2) <- dimnames(.ret)
      } else if (!identical(.dn, dimnames(.ret))) {
        stop("'cov' function must preserve matrix dimnames", call.=FALSE)
      }
      .ret <- .ret2
    }
  }
  if (any(.retF)) {
    class(.ret) <- c("lotriFix", class(.ret))
    attr(.ret, "lotriFix") <- .retF
  } else if (any(.retU)) {
    class(.ret) <- c("lotriFix", class(.ret))
    attr(.ret, "lotriUnfix") <- .retU
  }
  if (any(!is.na(env$labels))) {
    attr(.ret, "lotriLabels") <- env$labels
    if (!inherits(.ret, "lotriFix")) {
      class(.ret) <- c("lotriFix", class(.ret))
    }
  }
  if (.hasSame) {
    attr(.ret, "lotriSame") <- env$sameOff
    if (!inherits(.ret, "lotriFix")) {
      class(.ret) <- c("lotriFix", class(.ret))
    }
  }
  .ret
}
#' This modifies the call information to include the default arguments explicitly
#'
#' @param call call list to modify
#' @param cov Is this a covariance matrix (boolean/function; default=`FALSE`).
#' @param envir environment where lotri is evaluated
#' @param default default level of variability (id=default)
#' @return calling list incluing cov, envir and default
#' @noRd
#' @author Matthew L. Fidler
.lotriGetFullCall <- function(call, cov=FALSE, rcm=FALSE,
                              envir = parent.frame(),
                              default = "id") {
  .fullCall <- call
  if (!any(names(.fullCall) %in% "cov")) {
    .fullCall <- c(.fullCall, list(cov=cov))
  }
  if (!any(names(.fullCall) %in% "rcm")) {
    .fullCall <- c(.fullCall, list(rcm=rcm))
  }
  if (!any(names(.fullCall) %in% "default")) {
    .fullCall <- c(.fullCall, list(default=default))
  }
  if (!any(names(.fullCall) %in% "envir")) {
    .fullCall <- c(.fullCall, list(envir=envir))
  }
  .fullCall
}
#' This gets the cov information
#'
#' @param cov Is this a covariance matrix (boolean/function;
#'   default=`FALSE`).
#' @return list with cov and fun elements where cov is the original
#'   cov argument and fun is the function if cov was a function
#' @noRd
#' @author Matthew L. Fidler
.lotriCovInfo <- function(cov) {
  .fun <- NULL
  if (length(cov) != 1 || !is.logical(cov) || is.na(cov)) {
    if (is.function(cov)) {
      .fun <- cov
      cov <- TRUE
    } else {
      stop("'cov' must be a length 1 non-NA logical or function",
           call.=FALSE)
    }
  }
  list(cov=cov, fun=.fun)
}
#' This prepares the call for lotri by evaluating any subcalls
#'
#' @param call  lotri call to prepare
#' @param x lotri call x argument
#' @param xSub lotri call x argument subcall if it exists
#' @param envir environment where lotri is evaluated
#' @noRd
#' @author Matthew L. Fidler
.lotriPrepCall <- function(call, x, xSub, envir) {
  if (inherits(xSub, "{")) {
    x <- eval(parse(text=paste0("quote(", paste(deparse(xSub), collapse="\n"), ")")))
    call[[1]] <- x
  }
  .ncall <- names(call)
  if (any(.ncall == "envir")) {
    .w <- which(.ncall == "envir")
    call <- call[-.w]
  }
  .fullCnd <- NULL
  .fullCndLst <- list()
  if (length(call[[1]]) > 1 && identical(call[[1]][[1]], quote(`|`))) {
    .cnd <- call[[1]][[3]]
    .fullCndLst <- .parseCondition(.cnd, envir = envir)
    .fullCnd <- .fullCndLst[[1]]
    x <- eval(call[[1]][[2]], envir = envir)
  }
  list(call=call, x=x, fullCnd=.fullCnd, fullCndLst=.fullCndLst)
}
#' Expand conditionals in lotri calls and combine matrices
#'
#' This handles the conditionals in lotri and combines the matrices
#' from the conditionals with the main matrix and any subcalls. It
#' also handles the properties of the matrices and ensures they are
#' properly merged and amplified with defaults. The result is a list
#' of matrices that will be used to create the final lotri object.
#'
#'
#' @param env lotri environment with the main matrix and any
#'   conditional matrices
#' @param call lotri call to prepare
#' @param cov Is this a covariance matrix (boolean/function;
#'   default=`FALSE`).
#' @param rcm Do rcm re-ordering (boolean; default=`FALSE`).
#' @param default default level of variability (id=default)
#' @param envir environment where lotri is evaluated
#' @return list of matrices to be used for the final lotri object,
#'   with properties properly merged and amplified
#' @noRd
#' @author Matthew L. Fidler
.lotriExprCnd <- function(env, call, cov, rcm, default, envir) {
  .lstC <- list()
  .other <- NULL
  .prop <- NULL
  .ndef <- sum(names(call) %in% c("cov", "rcm", "default", "envir"))
  if (length(call) - .ndef > 1) {
    call <- call[-1]
    .other <- do.call("lotri",
                      .lotriGetFullCall(call, cov=cov, rcm=rcm,
                                        default=default, envir=envir),
                      envir=envir)
    if (inherits(.other, "lotri")) {
      .prop <- attr(.other, "lotri", exact=TRUE)
      class(.other) <- NULL
    }
  }
  if (any(env$cnd == default)) {
    .env2 <- env[[default]]
    .env2$isCov <- env$isCov
    .env2$rcm <- env$rcm
    .env2$fun <- env$fun
    .env2$df <- rbind(.env2$df, env$df)
    .env2$lastN <- 0
    .env2$names <- c(.env2$names, env$names)
    .env2$labels <- c(.env2$labels, env$labels)
    .env2$eta1 <- env$eta1 + .env2$eta1
  } else if (!is.null(env$df)) {
    env[[default]] <- new.env(parent=emptyenv())
    .env2 <- env[[default]]
    .env2$df <- env$df
    .env2$lastN <- 0
    .env2$isCov <- env$isCov
    .env2$rcm <- env$rcm
    .env2$fun <- env$fun
    .env2$eta1 <- env$eta1
    .env2$names <- env$names
    .env2$labels <- env$labels
    env$cnd <- c(default, env$cnd)
  }
  for (.j in env$cnd) {
    .env2 <- env[[.j]]
    .ret0 <- .lotriGetMatrixFromEnv(.env2, cnd=.j, fun=.env2$fun)
    .extra <- env[[paste0(.j, ".extra")]]
    if (!is.null(.extra)) {
      if (is.null(.prop) && any(names(.other) == .j)) {
        .prop <- dimnames(.other[[.j]])[[1]]
      }
      .prop <- .mergeProp(
        .prop, .j,
        .amplifyDefault(.extra, .env2$names)
      )
    }
    if (inherits(.other, "list") && any(names(.other) == .j)) {
      .fullCall <- .lotriGetFullCall(list(.ret0, .other[[.j]]),
                                     cov=cov,
                                     rcm=rcm,
                                     default=default,
                                     envir=envir)
      .ret0 <- do.call("lotri", .fullCall,
                       envir = envir)
      .other <- .other[names(.other) != .j]
    }
    .lstC[[.j]] <- .ret0
  }
  if (inherits(.other, "list")) {
    .lstC <- c(.lstC, .other)
  } else if (!is.null(.other)) {
    .lstC <- c(.lstC, list(.other))
  }
  if (!is.null(.prop)) {
    .prop <- .amplifyFinal(.lstC, .prop)
    attr(.lstC, "lotri") <- .prop
    class(.lstC) <- "lotri"
  }
  .lstC
}
#' Lotri expression result
#'
#' This function evaluates the lotri expression and returns the
#' resulting matrix, any fixed estimates, and whether the evaluation
#' is complete. It handles the parsing of the lotri expression,
#' including any conditionals, and prepares the final result for use
#' in creating a lotri object.
#'
#' @param sX represents the lotri expression to be evaluated,
#'   typically a call or expression object that defines the structure
#'   of the lotri model. This is the main input that contains the
#'   specifications for the matrices and any conditionals that need to
#'   be processed.
#' @param cov Is this a covariance matrix (boolean/function;
#'   default=`FALSE`).
#' @param rcm Do rcm re-ordering (boolean; default=`FALSE`).
#' @param fun If `cov` is a function, this is the function to apply to
#'   the matrix when `cov` is `TRUE`. It should take a matrix as input
#'   and return a matrix of the same dimensions. This allows for
#'   custom transformations of the covariance matrix if needed.
#' @param default default level of variability (id=default)
#' @param call the original call to lotri, used for error messages and
#'   to determine how to combine matrices if there are conditionals
#' @param envir environment where lotri is evaluated, used for
#'   evaluating any subcalls or conditions
#' @return the result of evaluating the lotri expression, which
#'   includes the resulting matrix (or list of matrices if there are
#'   conditionals), any fixed estimates, and a flag indicating whether
#'   the evaluation is complete. The result is typically a list with
#'   elements `ret` (the resulting matrix or list of matrices), `est`
#'   (data frame of fixed estimates), and `done` (boolean indicating
#'   if the evaluation is complete).
#' @noRd
#' @author Matthew L. Fidler
.lotriExprResult <- function(sX, cov, rcm, fun, default, call, envir) {
  .env <- new.env(parent = emptyenv())
  .env$isCov <- cov
  .env$fun <- fun
  .env$rcm <- rcm
  .env$df <- NULL
  .env$lastN <- 0
  .env$matrix <- NULL
  .env$eta1 <- 0L
  .env$cnd <- character()
  .envT <- .parseThetaEst(sX, .lotriParentEnv) # nolint
  .est <- .envT$df
  .env$.hasErr <- .envT$.hasErr
  .env$.err <- .envT$.err
  .env$.lines <- .envT$.lines
  ## the estimate names are needed while walking the `~` side so that
  ## `tka ~ 1` can be told apart from an eta specification
  .env$thetaNames <- .est$name
  .env$etaLhsNames <- .lotriAllEtaLhsNames(sX)
  .f(sX, .env)
  .printErr(.env) # nolint
  ## a shorthand normal prior is centered on the estimate, so `tka <-
  ## 0.45; tka ~ 1` is `dnorm(0.45, 1)`
  .thetaMeans <- NULL
  if (!is.null(.est)) {
    .thetaMeans <- setNames(as.double(.est$est), .est$name)
  }
  ## every normal prior shorthand is centered on what the model already
  ## says: the estimate for a theta name, the omega value for an `om.` one
  .omegaMeans <- .lotriOmegaDiagMeans(.env)
  .lotriThetaPriorsFromEnv(.env, means=.thetaMeans)
  .lotriThetaPriorsFromEnv(.env, "omegaPriorEnv", means=.omegaMeans)
  .lotriThetaPriorsFromEnv(.env, "jointPriorEnv",
                           means=c(.thetaMeans, .omegaMeans))
  ## `prior(tka) ~ 0.1` centers the same way, and can name either kind,
  ## so the omega means have to be reachable without the `om.` too
  .lotriThetaPriorsFromEnv(.env, "priorShorthandEnv",
                           means=.lotriPriorMeanAlias(c(.thetaMeans, .omegaMeans)))
  if (!is.null(.env$matrix)) {
    .res <- .lotriResolvePriors(.env$matrix, .est, .env$priors, .env$wholeOmegaPrior)
    return(list(ret=.res$ret, est=.res$est, done=TRUE))
  }
  if (length(.env$cnd) == 0L) {
    .ret <- .lotriGetMatrixFromEnv(.env, fun=.env$fun)
    .done <- FALSE
  } else {
    .ret <- .lotriExprCnd(.env, call, cov, rcm, default, envir)
    .done <- TRUE
  }
  ## resolved last so that priors are matched by name against the
  ## final (possibly `rcm` re-ordered) matrix
  .res <- .lotriResolvePriors(.ret, .est, .env$priors, .env$wholeOmegaPrior)
  list(ret=.res$ret, est=.res$est, done=.done)
}
#' Finalize the lotri expression result
#'
#' @param ret result to be finalized
#' @param est data frame of fixed estimates to be included in the
#'   final result
#' @param fullCnd full condition name if there was a conditional in
#'   the lotri expression, used for combining matrices and properties
#' @param fullCndLst list with full condition name and properties if
#'   there was a conditional in the lotri expression, used for
#'   combining matrices and properties
#' @param call the original call to lotri, used for determining how to
#'   combine matrices if there are conditionals
#' @param cov Is this a covariance matrix (boolean/function;
#'   default=`FALSE`), used for determining how to combine matrices if
#'   there are conditionals
#' @param rcm Do rcm re-ordering (boolean; default=`FALSE`), used for
#'   determining how to combine matrices if there are conditionals
#' @param default default level of variability (id=default), used for
#'   determining how to combine matrices if there are conditionals
#' @param envir environment where lotri is evaluated, used for
#'   evaluating any subcalls or conditions when combining matrices if
#'   there are conditionals
#' @return the finalized result of the lotri expression, which
#'   includes the resulting matrix (or list of matrices if there are
#'   conditionals) with any fixed estimates included and properties
#'   properly merged and amplified. The result is typically a list
#'   with elements `ret` (the resulting matrix or list of matrices),
#'   and `est` (data frame of fixed estimates), ready to be used for
#'   creating a lotri object.
#' @noRd
#' @author Matthew L. Fidler
.lotriFinalize <- function(ret, est, fullCnd, fullCndLst, call,
                           cov, rcm, default, envir) {
  if (!is.null(fullCnd)) {
    .lst <- list()
    .lst[[fullCnd]] <- ret
    .prop <- NULL
    if (!is.null(fullCndLst[[2]])) {
      .prop <- list()
      .prop[[fullCnd]] <- .amplifyDefault(
        fullCndLst[[2]],
        dimnames(ret)[[1]]
      )
    }
    if (!is.null(.prop)) {
      attr(.lst, "lotri") <- .amplifyFinal(.lst, .prop)
      class(.lst) <- "lotri"
    }
    if (length(call) == 1L) {
      return(.amplifyRetWithDfEst(.lst, est))
    }
    call <- call[-1]
    .fullCall <- .lotriGetFullCall(call,
                                   cov=cov,
                                   rcm=rcm,
                                   default=default,
                                   envir=envir)
    .tmp <- do.call("lotri", .fullCall, envir=envir)
    if (any(names(.tmp) == fullCnd)) {
      if (!is.null(.prop)) {
        .tmpL <- attr(.tmp, "lotri", exact=TRUE)
        .tmp0 <- .tmpL[[fullCnd]]
        .tmp1 <- .tmpL[names(.tmpL) != fullCnd]
        .prop <- .mergeProp(
          .prop, fullCnd,
          .amplifyDefault(
            .tmp0,
            dimnames(.tmp[[fullCnd]])[[1]]
          )
        )
        .prop <- c(.prop, .tmp1)
      }
      ret <- lotri(list(ret, .tmp[[fullCnd]]),
                   cov=cov, rcm=rcm, default=default, envir = envir)
      .w <- which(names(.tmp) != fullCnd)
      if (length(.w) > 0L) {
        .tmp <- .tmp[.w]
        .tmp2 <- list()
        .tmp2[[fullCnd]] <- ret
        ret <- c(.tmp2, .tmp)
        return(.amplifyRetWithDfEst(ret, est))
      } else {
        .tmp <- list()
        .tmp[[fullCnd]] <- ret
        if (!is.null(.prop)) {
          attr(.tmp, "lotri") <- .amplifyFinal(.tmp, .prop)
          class(.tmp) <- "lotri"
        }
        return(.amplifyRetWithDfEst(.tmp, est))
      }
    } else {
      .lst <- list()
      .lst[[fullCnd]] <- ret
      .tmpCnd <- c(.prop, attr(.tmp, "lotri", exact=TRUE))
      ret <- c(.lst, .tmp)
      if (!is.null(.tmpCnd)) {
        attr(ret, "lotri") <- .amplifyFinal(ret, .tmpCnd)
        class(ret) <- "lotri"
      }
      return(.amplifyRetWithDfEst(ret, est))
    }
  }
  .ndef <- sum(names(call) %in% c("cov", "rcm", "default", "envir"))
  if (length(call) - .ndef == 1L) {
    return(.amplifyRetWithDfEst(ret, est))
  }
  call <- call[-1]
  .fullCall <- .lotriGetFullCall(call,
                                 cov=cov,
                                 rcm=rcm,
                                 default=default,
                                 envir=envir)
  .tmp <- do.call("lotri", .fullCall, envir=envir)
  if (inherits(.tmp, "list")) {
    if (any(names(.tmp) == "")) {
      .w <- which(names(.tmp) == "")
      .lst <- list(ret, .tmp[[.w]])
      .fullCall <- .lotriGetFullCall(.lst,
                                     cov=cov,
                                     rcm=rcm,
                                     default=default,
                                     envir=envir)
      .tmp[[.w]] <- do.call("lotri", .fullCall, envir = envir)
      .amplifyRetWithDfEst(.tmp, est)
    } else {
      ret <- c(list(ret), .tmp)
      .amplifyRetWithDfEst(ret, est)
    }
  } else {
    ret <- lotri(c(list(ret), list(.tmp)),
                 cov=cov, rcm=rcm, default=default,
                 envir = envir)
    if (inherits(.tmp, "lotri")) {
      attr(ret, "lotri") <- .amplifyFinal(ret, attr(.tmp, "lotri", exact=TRUE))
      class(ret) <- "lotri"
    }
    .amplifyRetWithDfEst(ret, est)
  }
}

#' Easily Specify block-diagonal matrices with lower triangular info
#'
#' @param x list, matrix or expression, see details
#'
#' @param ... Other arguments treated as a list that will be
#'     concatenated then reapplied to this function.
#'
#' @param cov either a boolean or a function accepting a matrix input.
#'
#'   When a boolean, `cov` describes if this matrix definition is
#'   actually a rxode2/nlmixr2-style covariance matrix.
#'   If so, `lotri()` will enforce certain regularity conditions:
#'
#'   - When diagonal elements are zero, the off-diagonal elements are
#'     zero. This means the covariance element is fixed to zero and
#'     not truly part of the covariance matrix in general.
#'
#'   - For the rest of the matrix, `lotri` will check that it is
#'     non-positive definite (which is required for covariance matrix in
#'     general)
#'
#'   It is sometimes difficult to adjust covariance matrices to be
#'   non-positive definite.  For this reason `cov` may also be a
#'   function accepting a matrix input and returning a non-positive
#'   definite matrix from this matrix input.  When this is a function,
#'   it is equivalent to `cov=TRUE` with the additional ability to
#'   correct the matrix to be non-positive definite if needed.
#'
#' @param rcm logical; if `TRUE`, the matrix will be reordered to
#'   change the matrix to a banded matrix, which is easier to express
#'   in `lotri` than a full matrix.  The RCM stands for the reverse
#'   Cuthill McKee (RCM) algorithm which is used for this matrix permutation.
#'   (see `rcm()`)
#'
#' @inheritParams base::eval
#' @inheritParams as.lotri
#'
#' @return named symmetric matrix useful in `rxode2()` simulations (and
#'     perhaps elsewhere)
#'
#' @details
#'
#'  This can take an R matrix, a list including matrices or
#'  expressions, or expressions
#'
#'  Expressions can take the form
#'
#'  name ~ estimate
#'
#'  Or the lower triangular matrix when "adding" the names
#'
#'  name1 + name2 ~ c(est1,
#'                    est2, est3)
#'
#'  The matrices are concatenated into a block diagonal matrix, like
#'  \code{\link[Matrix]{bdiag}}, but allows expressions to specify
#'  matrices easier.
#'
#'  A block can be repeated, sharing one set of estimates, with
#'
#'  name3 + name4 ~ same()
#'
#'  This is NONMEM's \code{$OMEGA BLOCK(n) SAME}, and it is how an
#'  inter-occasion variability block is written when every occasion
#'  draws its own random effects from one shared covariance.
#'  \code{same()} repeats the immediately preceding *block* under new
#'  names; a further \code{same()} repeats that same original block
#'  rather than the copy, the way NONMEM chains \code{SAME}.  It takes
#'  no arguments, may be used with a condition
#'  (\code{name3 + name4 ~ same() | occ}), and inherits the fixed flags
#'  of the block it repeats.
#'
#'  \code{same()} looks back only within one \code{{}} block, and only
#'  at its own level of variability.  Each extra argument to
#'  \code{lotri()} is parsed by its own call, so
#'  \code{lotri(a + b ~ c(1, 0.1, 2), c1 + d1 ~ same())} has nothing to
#'  repeat; write the two lines in one \code{lotri({})} block instead.
#'
#'  In the data frame from \code{as.data.frame()} the repetition is
#'  recorded in the existing \code{condition} column rather than in a
#'  new column, naming the element that is mirrored:
#'  \code{"id:same:name1"} on a diagonal row and
#'  \code{"id:same:name1:name2"} on a covariance row.  Use
#'  \code{\link{lotriBaseCondition}} and its companions to read that
#'  column; comparing it directly (\code{condition == "id"}) will
#'  misclassify a repeated block.
#'
#'  This is distinct from the condition property \code{cnd(same = n)},
#'  which repeats a whole nesting level rather than one block, and which
#'  \code{\link{lotriSep}} uses.  The two compose.
#'
#'  Population estimates can be given with
#'
#'  name <- estimate
#'
#'  or with bounds, \code{name <- c(lower, estimate, upper)}
#'
#'  A prior distribution can be put on any of these with
#'
#'  prior(name) ~ dist(...)
#'
#'  Since the statement names what it applies to, prior lines can be
#'  put anywhere in the block.  A prior can be given for a population
#'  estimate, for a single eta, or for a whole covariance block:
#'
#'  prior(eta1, eta2) ~ lkjCorr(2)
#'
#'  Normal priors also have a shorthand that reuses the matrix syntax:
#'  when the name on the left of a \code{~} is a population estimate
#'  (instead of an eta), it is a normal prior with a zero mean and the
#'  given variance
#'
#'  tka ~ 4
#'
#'  tcl + tv ~ c(1,
#'               0.01, 1)
#'
#'  The first is a normal prior on \code{tka} with a standard deviation
#'  of 2 and the second a multivariate normal prior on \code{tcl} and
#'  \code{tv} with a zero mean vector.  Every matrix spelling works,
#'  including the per row line form and the \code{sd()}/\code{cor()}
#'  transformations.  The estimate given with \code{<-} stays the initial
#'  estimate; it is not the prior mean.
#'
#'  The distributions understood are listed by
#'  \code{\link{lotriPriorDists}}.  Each has three accepted spellings:
#'  the R name where R parameterizes it the same way 'Stan' does
#'  (\code{dnorm()}), the camelCase name (\code{invWishart()}), and the
#'  'Stan' name (\code{inv_wishart()}).  The canonical one is the R name
#'  where there is a faithful one and the camelCase name otherwise.
#'
#'  Bounds are not repeated in the prior; a parameter declared as
#'  \code{c(0, 1)} with a \code{dcauchy(0, 5)} prior is a half-Cauchy.
#'
#'  The scale matrix of the Wishart family is optional, since the block
#'  it is put on already is that matrix, so
#'  \code{prior(eta1, eta2) ~ invWishart(4)} gives just the degrees of
#'  freedom (the \code{$OMEGAPD} of a NONMEM NWPRI model).
#'
#'
#' @examples
#'
#' ## A few ways to specify the same matrix
#' lotri({et2 + et3 + et4 ~ c(40,
#'                            0.1, 20,
#'                            0.1, 0.1, 30)})
#'
#' ## You  do not need to enclose in {}
#' lotri(et2 + et3 + et4 ~ c(40,
#'                           0.1, 20,
#'                           0.1, 0.1, 30),
#'           et5 ~ 6)
#' ## But if you do enclose in {}, you can use
#' ## multi-line matrix specifications:
#'
#' lotri({et2 + et3 + et4 ~ c(40,
#'                            0.1, 20,
#'                            0.1, 0.1, 30)
#'           et5 ~ 6
#'           })
#'
#' ## A block can be repeated with `same()`, which is NONMEM's
#' ## `$OMEGA BLOCK(n) SAME`: one estimated 2x2 shared by three blocks,
#' ## the usual shape for correlated inter-occasion variability
#'
#' iov <- lotri({
#'   iov.cl1 + iov.v1 ~ c(0.1,
#'                        0.01, 0.2)
#'   iov.cl2 + iov.v2 ~ same()
#'   iov.cl3 + iov.v3 ~ same()
#' })
#'
#' iov
#'
#' ## the repetition rides in the `condition` column, so no column is
#' ## added to the data frame
#'
#' as.data.frame(iov)$condition
#'
#' ## You can also add lists or actual R matrices as in this example:
#' lotri(list(et2 + et3 + et4 ~ c(40,
#'                                0.1, 20,
#'                                0.1, 0.1, 30),
#'               matrix(1,dimnames=list("et5","et5"))))
#'
#' ## Overall this is a flexible way to specify symmetric block
#' ## diagonal matrices.
#'
#' ## For rxode2, you may also condition based on different levels of
#' ## nesting with lotri;  Here is an example:
#'
#' mat <- lotri(lotri(iov.Ka ~ 0.5,
#'                     iov.Cl ~ 0.6),
#'               lotri(occ.Ka ~ 0.5,
#'                     occ.Cl ~ 0.6) | occ(lower=4,nu=3))
#'
#' mat
#'
#' ## you may access features of the matrix simply by `$` that is
#'
#' mat$lower # Shows the lower bound for each condition
#'
#' mat$lower$occ # shows the lower bound for the occasion variable
#'
#' ## Note that `lower` fills in defaults for parameters.  This is true
#' ## for `upper` true;  In fact when accessing this the defaults
#' ## are put into the list
#'
#' mat$upper
#'
#' ## However all other values return NULL if they are not present like
#'
#' mat$lotri
#'
#' ## And values that are specified once are only returned on one list:
#'
#' mat$nu
#'
#' mat$nu$occ
#' mat$nu$id
#'
#' ## You can also change the default condition with `as.lotri`
#'
#' mat <- as.lotri(mat, default="id")
#'
#' mat
#'
#' @author Matthew L Fidler
#' @importFrom methods is
#' @importFrom stats setNames
#' @importFrom utils str
#' @export
lotri <- function(x, ..., cov=FALSE, rcm=FALSE,
                  envir = parent.frame(),
                  default = "id") {
  .covInfo <- .lotriCovInfo(cov)
  cov <- .covInfo$cov
  .fun <- .covInfo$fun
  if (missing(x)) {
    return(lotri({}, cov=cov, rcm=rcm, envir=envir, default=default))
  }
  if (is.null(.lotriParentEnv)) {
    assignInMyNamespace(".lotriParentEnv", envir)
    on.exit(assignInMyNamespace(".lotriParentEnv", NULL))
  }
  .call <- as.list(match.call())[-1]
  .xSub <- substitute(x)
  .prep <- .lotriPrepCall(.call, x, .xSub, envir)
  .call <- .prep$call
  x <- .prep$x
  .fullCnd <- .prep$fullCnd
  .fullCndLst <- .prep$fullCndLst
  .est <- NULL
  if (is.null(x)) {
    .ret <- NULL
  } else if (is.list(x)) {
    .ret <- .lotriList(x, ..., envir = envir)
  } else if (is.matrix(x)) {
    .ret <- x
  } else {
    if (is.call(.xSub) &&
          identical(.xSub[[1]], quote(`quote`)) &&
          (!is.call(.xSub[[2]]) || !identical(.xSub[[2]][[1]], quote(`{`)))) {
      stop("bad matrix specification", call. = FALSE)
    }
    .sX <- substitute(x)
    if (is.call(.sX) && identical(.sX[[1]], quote(`[[`))) {
      .sX <- x
    }
    .res <- .lotriExprResult(.sX, cov, rcm, .fun, default, .call, envir)
    .ret <- .res$ret
    .est <- .res$est
    if (.res$done) {
      return(.amplifyRetWithDfEst(.ret, .est))
    }
  }
  .lotriFinalize(.ret, .est, .fullCnd, .fullCndLst, .call,
                 cov, rcm, default, envir)
}

#' @importFrom utils .DollarNames
#' @export
.DollarNames.lotri <- function(x, pattern) { # nolint
  grep(pattern, unique(c(
    names(x), ".allNames", ".bounds",
    ".names", ".list", ".maxNu", x$.names
  )),
  value = TRUE
  )
}

#' @export
`$.lotri` <- function(obj, arg, exact = FALSE) {
  .lotri <- attr(obj, "lotri", exact=TRUE)
  if (arg == ".maxNu") {
    return(.Call(`_lotriMaxNu`, # nolint
                 obj, PACKAGE = "lotri"))
  }
  if (any(names(obj) == arg)) {
    .tmp <- obj
    class(.tmp) <- NULL
    return(.tmp[[arg]])
  }
  if (arg == ".names") {
    return(unique(unlist(lapply(
      names(obj),
      function(x) {
        names(.lotri[[x]])
      }
    ))))
  }
  if (arg == ".allNames") {
    return(.Call(`_lotriAllNames`, # nolint
                 obj, PACKAGE = "lotri"))
  }
  if (arg == ".bounds") {
    return(.Call(`_lotriGetBounds`, # nolint
                 obj, NULL, 1L, PACKAGE = "lotri"))
  }
  if (arg == ".list") {
    .tmp <- obj
    class(.tmp) <- NULL
    attr(.tmp, "lotri") <- NULL
    .names <- obj$.names
    for (.n in .names) {
      if (!any(.n == names(.tmp))) {
        .tmp[[.n]] <- `$.lotri`(obj, .n) # nolint
      }
    }
    return(.tmp)
  }

  .env <- new.env(parent = emptyenv())
  .env$empty <- TRUE
  .ret <- setNames(lapply(names(obj), function(x) {
    if (any(names(.lotri) == x)) {
      .ret <- .lotri[[x]][[arg]]
      if (is.null(.ret)) {
        return(NULL)
      }
      assign("empty", FALSE, .env)
      .ret
    } else {
      .def <- .defaultProperties[arg]
      if (!is.na(.def)) {
        .w <- which(names(obj) == x)
        if (length(.w) == 1) {
          .dim <- dimnames(obj[[.w]])[[1]]
          .ret <- setNames(rep(.def, length(.dim)), .dim)
          return(.ret)
        }
      }
      NULL
    }
  }), names(obj))
  .w <- which(unlist(lapply(.ret, is.null)))
  if (length(.w) > 0) {
    .ret <- .ret[-.w]
  }
  if (.env$empty) {
    .def <- .defaultProperties[arg]
    if (!is.na(.def)) {
      .ret <- setNames(lapply(names(obj), function(x) {
        .dim <- dimnames(obj[[x]])[[1]]
        setNames(rep(.def, length(.dim)), .dim)
      }), names(obj))
      return(.ret)
    }
    return(NULL)
  }
  .ret
}

#' @export
as.matrix.lotri <- function(x, ...) {
  .ret <- x
  class(.ret) <- NULL
  if (length(.ret) == 1) {
    .ret[[1]]
  } else {
    stop("cannot convert multiple level lotri matrix to simple matrix", call. = FALSE)
  }
}
#' Create a matrix from a list of matrices
#'
#' This creates a named banded symmetric matrix from a list of named
#' symmetric matrices.
#'
#' @param matList list of symmetric named matrices
#'
#' @param format The format of dimension names when a sub-matrix is
#'   repeated. The format will be called with the dimension number,
#'   so "ETA[\%d]" would represent "ETA[1]", "ETA[2]", etc
#'
#' @param start The number the counter of each repeated dimension
#'   should start.
#'
#' @return Named symmetric block diagonal matrix based on
#'   concatenating the list of matrices together
#'
#' @examples
#'
#' testList <- list(lotri({et2 + et3 + et4 ~ c(40,
#'                            0.1, 20,
#'                            0.1, 0.1, 30)}),
#'                  lotri(et5 ~ 6))
#'
#' testList
#'
#' lotriMat(testList)
#'
#'
#' # Another option is to repeat a matrix a number of times.  This
#' # can be done with list(matrix, # times to repeat).
#'
#' # In the example below, the first matrix is repeated 3 times
#' testList <- list(list(lotri({et2 + et3 + et4 ~ c(40,
#'                            0.1, 20,
#'                            0.1, 0.1, 30)}), 3),
#'                  lotri(et5 ~ 6))
#'
#' lotriMat(testList)
#'
#' # Notice that the dimension names `et2`, `et3` and `et4` are
#' # repeated.
#'
#' # Another option is to name the dimensions.  For example it could
#' # be `ETA[1]`, `ETA[2]`, etc by using the 'format' option:
#'
#' lotriMat(testList, "ETA[%d]")
#'
#' # Or could start with ETA[2]:
#'
#' lotriMat(testList, "ETA[%d]", 2)
#'
#' @author Matthew Fidler
#' @export
lotriMat <- function(matList, format = NULL, start = 1L) {
  .Call(`_lotriLstToMat`, # nolint
        matList, format, start, class(matrix(0)), PACKAGE = "lotri")
}

#' Separate a lotri matrix into above and below lotri matrices
#'
#' This is used for creating nesting simulations in `rxode2()` and may
#' not be useful for external function calls.
#'
#' @param x lotri matrix
#'
#' @param above Named integer vector listing variability above the id
#'   level.  Each element lists the number of population differences
#'   in the whole data-set (as integer)
#'
#' @param below Named integer vector listing variability below the id
#'   level.  Each element lists the number of items below the
#'   individual level.  For example with 3 occasions per individual
#'   you could use 'c(occ=3L)'
#'
#' @param aboveStart Add the attribute of where THETA[#] will be added
#'
#' @param belowStart Add the attribute of where ETA[#] will be added
#'
#' @return List of two lotri matrices
#'
#' @author Matthew Fidler
#'
#' @export
#'
#' @examples
#'
#' omega <- lotri(lotri(eta.Cl ~ 0.1,
#'                         eta.Ka ~ 0.1) | id(nu=100),
#'                   lotri(eye.Cl ~ 0.05,
#'                         eye.Ka ~ 0.05) | eye(nu=50),
#'                   lotri(iov.Cl ~ 0.01,
#'                         iov.Ka ~ 0.01) | occ(nu=200),
#'                   lotri(inv.Cl ~ 0.02,
#'                         inv.Ka ~ 0.02) | inv(nu=10))
#'
#' lotriSep(omega, above=c(inv=10L), below=c(eye=2L, occ=4L))
lotriSep <- function(x, above, below,
                     aboveStart = 1L, belowStart = 1L) {
  .Call(`_lotriSep`, # nolint
        x, above, below, aboveStart = as.integer(aboveStart),
        belowStart = as.integer(belowStart), PACKAGE = "lotri")
}
