##' @useDynLib lotri, .registration = TRUE
NULL

##' Parse lower triangular matrix list
##'
##' This is for x~c(1..) or x1+x2~c(...)
##' @title
##' @param x2 Second element of parsing list ie  `x` or `x1+x2`
##' @param x3 Third element of list; ie c(...)
##' @param env  environment to update
##' @return Nothing; updates environment
##' @author Matthew Fidler
##' @noRd
.lotri1 <- function(x2, x3, env) {
  env$netas <- length(x3) - 1
  .num <- sqrt(1 + env$netas * 8) / 2 - 1 / 2
  if (round(.num) == .num) {
    .n <- unlist(strsplit(as.character(x2), " +[+] +"))
    .n <- .n[.n != "+"]
    if (length(.n) == .num) {
      env$names <- c(env$names, .n)
      .r <- x3[-1]
      .r <- unlist(lapply(.r, function(x) {
        return(as.numeric(eval(x)))
      }))
      .i <- 0
      .j <- 1
      for (.k in seq_along(.r)) {
        .v <- .r[.k]
        .i <- .i + 1
        if (.i == .j) {
          env$df <- rbind(
            env$df,
            data.frame(i = env$eta1 + .i, j = env$eta1 + .i, x = .v)
          )
          .j <- .j + 1
          .i <- 0
        } else {
          env$df <- rbind(
            env$df,
            data.frame(
              i = c(env$eta1 + .i, env$eta1 + .j),
              j = c(env$eta1 + .j, env$eta1 + .i), x = .v
            )
          )
        }
      }
      env$eta1 <- env$eta1 + .num
    } else {
      stop("number of items and lower triangular matrix mismatch", call. = FALSE)
    }
  } else {
    stop("matrix expression should be 'name ~ c(lower-tri)'", call. = FALSE)
  }
}

##' DSL parsing function
##'
##' @param x Parsing tree
##' @param env environment to update
##' @return Nothing
##' @author Matthew Fidler
##' @noRd
.f <- function(x, env) {
  if (is.name(x)) {
    return(character())
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`~`))) {
      if (length(x[[3]]) == 1) {
        ## et1 ~ 0.2
        env$netas <- 1
        env$eta1 <- env$eta1 + 1
        env$names <- c(env$names, as.character(x[[2]]))
        env$df <- rbind(
          env$df,
          data.frame(
            i = env$eta1,
            j = env$eta1,
            x = as.numeric(eval(x[[3]]))
          )
        )
      } else {
        ## et1+et2+et3~NULL lower triangular matrix
        ## Should fixed be allowed????
        if (any(tolower(as.character(x[[3]][[1]])) == c("c", "fix", "fixed"))) {
          if (any(tolower(as.character(x[[3]][[1]])) == c("fix", "fixed"))) {
            stop("fix/fixed are not allowed with lotri matrix specifications", call. = FALSE)
          }
          .lotri1(x[[2]], x[[3]], env)
        } else {
          .val <- try(eval(x[[3]]), silent = TRUE)
          if (is.numeric(.val) || is.integer(.val)) {
            env$netas <- 1
            env$eta1 <- env$eta1 + 1
            env$names <- c(env$names, as.character(x[[2]]))
            env$df <- rbind(
              env$df,
              data.frame(i = env$eta1, j = env$eta1, x = .val)
            )
          } else {
            .cnd <- try(as.character(x[[3]][[1]]), silent = TRUE)
            .didCnd <- FALSE
            if (inherits(.cnd, "character")) {
              if (.cnd == "|") {
                .cnd <- x[[3]][[3]]
                .cndFull <- .parseCondition(.cnd, envir = env)
                .cnd <- .cndFull[[1]]
                ## Each condition is parsed so this new environment
                ## should not be elsewhere
                .env2 <- new.env(parent = emptyenv())
                .env2$df <- NULL
                .env2$eta1 <- 0L
                env$cnd <- unique(c(env$cnd, .cnd))
                env[[.cnd]] <- .env2
                env[[paste0(.cnd, ".extra")]] <- .cndFull[[2]]
                .val <- try(eval(x[[3]][[2]]), silent = TRUE)
                if ((length(.val) == 1) &&
                  (is.numeric(.val) || is.integer(.val))) {
                  .env2$netas <- 1
                  .env2$eta1 <- .env2$eta1 + 1
                  .env2$names <- c(.env2$names, as.character(x[[2]]))
                  .env2$df <- rbind(
                    .env2$df,
                    data.frame(
                      i = .env2$eta1, j = .env2$eta1,
                      x = .val
                    )
                  )
                } else {
                  .lotri1(x[[2]], x[[3]][[2]], .env2)
                }
                .didCnd <- TRUE
              }
            }
            if (!.didCnd) {
              stop("matrix expression should be 'name ~ c(lower-tri)'", call. = FALSE)
            }
          }
        }
      }
    } else if (identical(x[[1]], quote(`{`))) {
      lapply(x, .f, env = env)
    } else if (identical(x[[1]], quote(`quote`))) {
      lapply(x[[2]], .f, env = env)
    } else {
      stop("matrix expression should be 'name ~ c(lower-tri)'", call. = FALSE)
    }
  } else {
    ## is.pairlist OR is.atomic OR unknown...
    stop("bad matrix specification", call. = FALSE)
  }
}
##' Parses condition
##'
##' @param cond Condition parsing tree
##' @param envir Environment to parse condition in.
##'
##' @return list with 2 elements: - First element is the name of the condition - Second element is extra information
##' @author Matthew Fidler
##' @noRd
.parseCondition <- function(cond, envir = parent.frame()) {
  if (length(cond) == 1) {
    .fullCnd <- as.character(cond)
    return(list(.fullCnd, NULL))
  }
  .fullCnd <- as.character(cond[[1]])
  if (regexpr("^[a-zA-Z][a-zA-Z0-9_.]*$", .fullCnd) == -1) {
    stop("unsupported conditional statement", call. = FALSE)
  }
  .env <- list2env(as.list(envir), parent = globalenv())
  .env[[.fullCnd]] <- function(...) {
    return(list(...))
  }
  .prop <- eval(cond, envir = .env)
  return(list(.fullCnd, .prop))
}

.defaultProperties <- c(lower = -Inf, upper = Inf)

##' Amplify Default properties
##'
##' @param prop proprety list where `.defaultProperties` will be amplified
##' @param names names of matrix components to check against
##' @return Amplified property list
##' @author Matthew Fidler
##' @noRd
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
  return(.newProp)
}
##' Amplifies final lotri list with defaults in .defaultProperties
##'
##' @param finalList Final List before return
##' @param prop current properties
##' @return lotri amplified with defaults for all parameters
##' @author Matthew Fidler
##' @noRd
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
  return(prop)
}
##' Merge properties between two matrices
##'
##' @param prop Initial property list or character vector of names to
##'   apply default properties on...
##' @param id ID of the matrix with more properites
##' @param new new properites of the matrix
##' @return A merged property that will be used for lotri composite
##'   matrices
##' @author Matthew Fidler
##' @noRd
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
  return(.ret)
}

##' Extract a matrix saved in the environment
##'
##' @param env Environment where matrix is saved
##' @param val value where the matrix is saved in
##' @return named matrix
##' @author Matthew Fidler
##' @noRd
.getMatrix <- function(env, val) {
  return(.Call(`_lotriLstToMat`, env[[val]], NULL, 1L, PACKAGE = "lotri"))
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
          assign("...empty", c(.env[["...empty"]], list(.cur)), .env)
        } else {
          assign(.curName, c(.env[[.curName]], list(.cur)), .env)
          assign("...cnd", unique(c(.env[["...cnd"]], .curName)), .env)
        }
      } else if (inherits(.cur, "list") || inherits(.cur, "lotri")) {
        lapply(
          seq_along(.cur),
          function(y) {
            .cury <- .cur[[y]]
            .curName <- names(.cur)[y]
            if (.curName == "") {
              assign("...empty", c(
                .env[["...empty"]],
                list(.cury)
              ), .env)
            } else {
              assign(.curName, list(.cury), .env)
              assign("...cnd", unique(c(
                .env[["...cnd"]],
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
##' ## But if you do enclose in {}, you can use
##' ## multi-line matrix specifications:
##'
##' lotri({et2 + et3 + et4 ~ c(40,
##'                            0.1, 20,
##'                            0.1, 0.1, 30)
##'           et5 ~ 6
##'           })
##'
##' ## You can also add lists or actual R matrices as in this example:
##' lotri(list(et2 + et3 + et4 ~ c(40,
##'                                0.1, 20,
##'                                0.1, 0.1, 30),
##'               matrix(1,dimnames=list("et5","et5"))))
##'
##' ## Overall this is a flexible way to specify symmetric block
##' ## diagonal matrices.
##'
##' ## For RxODE, you may also condition based on different levels of
##' ## nesting with lotri;  Here is an example:
##'
##' mat <- lotri(lotri(iov.Ka ~ 0.5,
##'                     iov.Cl ~ 0.6),
##'               lotri(occ.Ka ~ 0.5,
##'                     occ.Cl ~ 0.6) | occ(lower=4,nu=3))
##'
##' mat
##'
##' ## you may access features of the matrix simply by `$` that is
##'
##' mat$lower # Shows the lower bound for each condition
##'
##' mat$lower$occ # shows the lower bound for the occasion variable
##'
##' ## Note that `lower` fills in defaults for parameters.  This is true
##' ## for `upper` true;  In fact when accessing this the defaults
##' ## are put into the list
##'
##' mat$upper
##'
##' ## However all other values return NULL if they are not present like
##'
##' mat$lotri
##'
##' ## And values that are specified once are only returned on one list:
##'
##' mat$nu
##'
##' mat$nu$occ
##' mat$nu$id
##'
##' ## You can also change the default condition with `as.lotri`
##'
##' mat <- as.lotri(mat,default="id")
##'
##' mat
##'
##' @author Matthew L Fidler
##' @importFrom methods is
##' @importFrom stats setNames
##' @importFrom utils str
##' @export
lotri <- function(x, ..., envir = parent.frame()) {
  .call <- as.list(match.call())[-1]
  .ncall <- names(.call)
  if (any(.ncall == "envir")) {
    .w <- which(.ncall == "envir")
    .call <- .call[-.w]
  }
  .fullCnd <- NULL
  .fullCndLst <- list()
  if (length(.call[[1]]) > 1) {
    if (identical(.call[[1]][[1]], quote(`|`))) {
      .cnd <- .call[[1]][[3]]
      .fullCndLst <- .parseCondition(.cnd, envir = envir)
      .fullCnd <- .fullCndLst[[1]]
      x <- eval(.call[[1]][[2]], envir = envir)
    }
  }
  if (is.null(x)) {
    .ret <- NULL
  } else if (is.list(x)) {
    .ret <- .lotriList(x, ..., envir = envir)
  } else if (is.matrix(x)) {
    .ret <- x
  } else {
    .env <- new.env(parent = emptyenv())
    .env$df <- NULL
    .env$eta1 <- 0L
    .env$cnd <- character()
    .sX <- substitute(x)
    if (is.call(.sX)) {
      if (identical(.sX[[1]], quote(`[[`))) {
        .sX <- x
      }
    }
    .f(.sX, .env)
    if (length(.env$cnd) == 0L) {
      .ret <- diag(.env$eta1)
      for (.i in seq_along(.env$df$i)) {
        .ret[.env$df$i[.i], .env$df$j[.i]] <- .env$df$x[.i]
      }
      dimnames(.ret) <- list(.env$names, .env$names)
    } else {
      .lstC <- list()
      .other <- NULL
      .prop <- NULL
      if (length(.call) > 1) {
        .call <- .call[-1]
        .other <- do.call("lotri", .call, envir = envir)
        if (inherits(.other, "lotri")) {
          .prop <- attr(.other, "lotri")
          class(.other) <- NULL
        }
      }
      for (.j in .env$cnd) {
        .env2 <- .env[[.j]]
        .ret0 <- diag(.env2$eta1)
        for (.i in seq_along(.env2$df$i)) {
          .ret0[.env2$df$i[.i], .env2$df$j[.i]] <- .env2$df$x[.i]
        }
        dimnames(.ret0) <- list(.env2$names, .env2$names)
        .extra <- .env[[paste0(.j, ".extra")]]
        if (!is.null(.extra)) {
          if (is.null(.prop)) {
            if (any(names(.other) == .j)) {
              .prop <- dimnames(.other[[.j]])[[1]]
            }
          }
          .prop <- .mergeProp(
            .prop, .j,
            .amplifyDefault(.extra, .env2$names)
          )
        }
        if (inherits(.other, "list")) {
          if (any(names(.other) == .j)) {
            .ret0 <- do.call("lotri", list(.ret0, .other[[.j]],
              envir = envir
            ),
            envir = envir
            )
            .other <- .other[names(.other) != .j]
          }
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
      return(.lstC)
    }
  }
  if (!is.null(.fullCnd)) {
    .lst <- list()
    .lst[[.fullCnd]] <- .ret
    .prop <- NULL
    if (!is.null(.fullCndLst[[2]])) {
      .prop <- list()
      .prop[[.fullCnd]] <- .amplifyDefault(
        .fullCndLst[[2]],
        dimnames(.ret)[[1]]
      )
    }
    if (!is.null(.prop)) {
      attr(.lst, "lotri") <- .amplifyFinal(.lst, .prop)
      class(.lst) <- "lotri"
    }
    if (length(.call) == 1L) {
      return(.lst)
    }
    .call <- .call[-1]
    .tmp <- do.call("lotri", .call, envir = envir)
    if (any(names(.tmp) == .fullCnd)) {
      if (!is.null(.prop)) {
        .tmpL <- attr(.tmp, "lotri")
        .tmp0 <- .tmpL[[.fullCnd]]
        .tmp1 <- .tmpL[names(.tmpL) != .fullCnd]
        .prop <- .mergeProp(
          .prop, .fullCnd,
          .amplifyDefault(
            .tmp0,
            dimnames(.tmp[[.fullCnd]])[[1]]
          )
        )
        .prop <- c(.prop, .tmp1)
      }
      .ret <- lotri(list(.ret, .tmp[[.fullCnd]]), envir = envir)
      .w <- which(names(.tmp) != .fullCnd)
      if (length(.w) > 0L) {
        .tmp <- .tmp[.w]
        .tmp2 <- list()
        .tmp2[[.fullCnd]] <- .ret
        .ret <- c(.tmp2, .tmp)
        return(.ret)
      } else {
        .tmp <- list()
        .tmp[[.fullCnd]] <- .ret
        if (!is.null(.prop)) {
          attr(.tmp, "lotri") <- .amplifyFinal(.tmp, .prop)
          class(.tmp) <- "lotri"
        }
        return(.tmp)
      }
    } else {
      .lst <- list()
      .lst[[.fullCnd]] <- .ret
      .tmpCnd <- c(.prop, attr(.tmp, "lotri"))
      .ret <- c(.lst, .tmp)
      if (!is.null(.tmpCnd)) {
        attr(.ret, "lotri") <- .amplifyFinal(.ret, .tmpCnd)
        class(.ret) <- "lotri"
      }
      return(.ret)
    }
  } else {
    if (length(.call) == 1L) {
      return(.ret)
    }
    .call <- .call[-1]
    .tmp <- do.call("lotri", .call, envir = envir)
    if (inherits(.tmp, "list")) {
      if (any(names(.tmp) == "")) {
        .w <- which(names(.tmp) == "")
        .lst <- list(.ret, .tmp[[.w]], envir = envir)
        .tmp[[.w]] <- do.call("lotri", .lst, envir = envir)
        return(.tmp)
      } else {
        .ret <- c(list(.ret), .tmp)
        return(.ret)
      }
    } else {
      .ret <- lotri(c(list(.ret), list(.tmp)), envir = envir)
      if (inherits(.tmp, "lotri")) {
        attr(.ret, "lotri") <- .amplifyFinal(.ret, attr(.tmp, "lotri"))
        class(.ret) <- "lotri"
      }
      return(.ret)
    }
  }
}

##' @export
print.lotri <- function(x, ...) {
  .tmp <- x
  .lotri <- attr(.tmp, "lotri")
  class(.tmp) <- NULL
  attr(.tmp, "lotri") <- NULL
  print(.tmp)
  .names <- x$.names
  if (length(.names) > 0) {
    cat(paste0("Properties: ", paste(.names, collapse = ", ")), "\n")
  }
  return(invisible(x))
}

##' @export
str.lotri <- function(object, ...) {
  str(object$.list)
}

##' @importFrom utils .DollarNames
##' @export
.DollarNames.lotri <- function(x, pattern) {
  grep(pattern, unique(c(
    names(x), ".allNames", ".bounds",
    ".names", ".list", ".maxNu", x$.names
  )),
  value = TRUE
  )
}

##' @export
`$.lotri` <- function(obj, arg, exact = FALSE) {
  .lotri <- attr(obj, "lotri")
  if (arg == ".maxNu") {
    return(.Call(`_lotriMaxNu`, obj, PACKAGE = "lotri"))
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
    return(.Call(`_lotriAllNames`, obj, PACKAGE = "lotri"))
  }
  if (arg == ".bounds") {
    return(.Call(`_lotriGetBounds`, obj, NULL, 1L, PACKAGE = "lotri"))
  }
  if (arg == ".list") {
    .tmp <- obj
    class(.tmp) <- NULL
    attr(.tmp, "lotri") <- NULL
    .names <- obj$.names
    for (.n in .names) {
      if (!any(.n == names(.tmp))) {
        .tmp[[.n]] <- `$.lotri`(obj, .n)
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
      return(.ret)
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
      return(NULL)
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
  return(.ret)
}

##' As lower triangular matrix
##'
##' @param x Matrix or other data frame
##'
##' @param ... Other factors
##'
##' @param default Is the default factor when no conditioning is
##'     implemented.
##'
##' @return Lower triangular matrix
##'
##' @author Matthew Fidler
##'
##' @export
as.lotri <- function(x, ..., default = "") {
  UseMethod("as.lotri")
}

##' @rdname as.lotri
##' @export
as.lotri.matrix <- function(x, ..., default = "") {
  .Call(`_asLotriMat`, x, list(...), default = default)
}

##' @rdname as.lotri
##' @export
as.lotri.default <- function(x, ..., default = "") {
  .ret <- x
  class(.ret) <- NULL
  .n <- names(.ret)
  .w <- which(names(.ret) == "")
  if (length(.w) == 1) {
    .n[.w] <- default
    names(.ret) <- .n
  }
  class(.ret) <- "lotri"
  return(.ret)
}

##' @export
as.matrix.lotri <- function(x, ...) {
  .ret <- x
  class(.ret) <- NULL
  if (length(.ret) == 1) {
    return(.ret[[1]])
  } else {
    stop("cannot convert multiple level lotri matrix to simple matrix", call. = FALSE)
  }
}
##' Create a matrix from a list of matrices
##'
##' This creates a named banded symmetric matrix from a list of named
##' symmetric matrices.
##'
##' @param matList list of symmetric named matrices
##'
##' @param format The format of dimension names when a sub-matrix is
##'   repeated. The format will be called with the dimension number,
##'   so "ETA[\%d]" would represent "ETA[1]", "ETA[2]", etc
##'
##' @param start The number the counter of each repeated dimension
##'   should start.
##'
##' @return Named symmetric block diagonal matrix based on
##'   concatenating the list of matrices together
##'
##' @examples
##'
##' testList <- list(lotri({et2 + et3 + et4 ~ c(40,
##'                            0.1, 20,
##'                            0.1, 0.1, 30)}),
##'                  lotri(et5 ~ 6))
##'
##' testList
##'
##' lotriMat(testList)
##'
##'
##' # Another option is to repeat a matrix a number of times.  This
##' # can be done with list(matrix, # times to repeat).
##'
##' # In the example below, the first matrix is repeated 3 times
##' testList <- list(list(lotri({et2 + et3 + et4 ~ c(40,
##'                            0.1, 20,
##'                            0.1, 0.1, 30)}), 3),
##'                  lotri(et5 ~ 6))
##'
##' lotriMat(testList)
##'
##' # Notice that the dimension names `et2`, `et3` and `et4` are
##' # repeated.
##'
##' # Another option is to name the dimensions.  For example it could
##' # be `ETA[1]`, `ETA[2]`, etc by using the 'format' option:
##'
##' lotriMat(testList, "ETA[%d]")
##'
##' # Or could start with ETA[2]:
##'
##' lotriMat(testList, "ETA[%d]", 2)
##'
##' @author Matthew Fidler
##' @export
lotriMat <- function(matList, format = NULL, start = 1L) {
  .Call(`_lotriLstToMat`, matList, format, start, PACKAGE = "lotri")
}

##' Separate a lotri matrix into above and below lotri matrices
##'
##' This is used for creating nesting simulations in `RxODE` and may
##' not be useful for external function calls.
##'
##' @param x lotri matrix
##' @param above Named integer vector listing variability above the id
##'   level.  Each element lists the number of population differences
##'   in the whole data-set (as integer)
##' @param below Named integer vector listing variability below the id
##'   level.  Each element lists the number of items below the
##'   individual level.  For example with 3 occasions per indivdiual
##'   you could use 'c(occ=3L)'
##' @param aboveStart Add the attribute of where THETA[#] will be added
##' @param belowStart Add the attribute of where ETA[#] will be added
##' @return List of two lotri matrices
##' @author Matthew Fidler
##' @export
##' @examples
##' omega <- lotri(lotri(eta.Cl ~ 0.1,
##'                         eta.Ka ~ 0.1) | id(nu=100),
##'                   lotri(eye.Cl ~ 0.05,
##'                         eye.Ka ~ 0.05) | eye(nu=50),
##'                   lotri(iov.Cl ~ 0.01,
##'                         iov.Ka ~ 0.01) | occ(nu=200),
##'                   lotri(inv.Cl ~ 0.02,
##'                         inv.Ka ~ 0.02) | inv(nu=10))
##'
##' lotriSep(omega, above=c(inv=10L), below=c(eye=2L, occ=4L))
lotriSep <- function(x, above, below,
                     aboveStart = 1L, belowStart = 1L) {
  .Call(`_lotriSep`, x, above, below,
    aboveStart = as.integer(aboveStart), belowStart = as.integer(belowStart),
    PACKAGE = "lotri"
  )
}
