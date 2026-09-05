## Declared non-Gaussian random effect (eta) distributions.
##
## An eta is Normal by construction everywhere in the nlmixr2 stack.  A
## `dist()` line says it is not:
##
##   dist(eta.cl) ~ dgamma(shape=1/exp(lrv), rate=1/(exp(lrv)*exp(lm)))
##
## The declaration is *stored*, validated and round-tripped here; it is
## turned into model code downstream (in 'rxode2'), the same division of
## labour the `prior()` lines already use.
##
## The technique this serves is Bauer's (NONMEM 7.5.1): keep the latent
## random effect standard normal and change the CDF,
##
##   z   ~ N(0, 1)        latent
##   u   = Phi(z)         normal CDF   ->  U(0, 1)
##   eta = Q(u; args)     inverse CDF of the declared family
##
## which is why a declared eta's variance is not a variance any more: the
## latent scale is standard normal, so the block it lives in has to be a
## CORRELATION matrix (a Gaussian copula) and its diagonal has to be one.
## That is checked in `.lotriResolveEtaDists()`.

#' Inverse CDF templates for the families that can be declared on an eta
#'
#' Each entry is `name|template`, where the template is an 'rxode2'
#' expression with `{u}` standing for the uniform value `Phi(z)` and
#' `{par}` for the distribution's parameter of that name.  A family in
#' `lotriPriorDists()` with no entry here cannot be declared on an eta:
#' its quantile function is neither elementary nor available as an
#' 'rxode2' function.
#'
#' The parameterizations are the catalog's, so `lotriPriorDists()` remains
#' the single source of truth for argument names and order.
#'
#' @noRd
#' @author Matthew L. Fidler
.lotriEtaDistDefs <- c(
  ## unbounded continuous
  "dnorm|({mean}) + ({sd})*qnorm({u})",
  "stdNormal|qnorm({u})",
  "studentT|({mu}) + ({sigma})*studentTInv({u}, {nu})",
  "dcauchy|({location}) + ({scale})*tan(pi*(({u}) - 0.5))",
  "doubleExponential|({mu}) - ({sigma})*sign(({u}) - 0.5)*log1p(-2*sign(({u}) - 0.5)*(({u}) - 0.5))",
  "dlogis|({location}) + ({scale})*log(({u})/(1 - ({u})))",
  "gumbel|({mu}) - ({beta})*log(-log({u}))",
  ## positive continuous
  "dlnorm|exp(({meanlog}) + ({sdlog})*qnorm({u}))",
  "dchisq|2*gammapInv(({df})/2, {u})",
  "invChiSquare|1/(2*gammapInv(({nu})/2, 1 - ({u})))",
  "scaledInvChiSquare|({nu})*({sigma})*({sigma})/(2*gammapInv(({nu})/2, 1 - ({u})))",
  "dexp|-log1p(-({u}))/({rate})",
  "dgamma|gammapInv({shape}, {u})/({rate})",
  "invGamma|({beta})/gammapInv({alpha}, 1 - ({u}))",
  "dweibull|({scale})*(-log1p(-({u})))^(1/({shape}))",
  "frechet|({sigma})*(-log({u}))^(-1/({alpha}))",
  "rayleigh|({sigma})*sqrt(-2*log1p(-({u})))",
  "pareto|({y_min})*(1 - ({u}))^(-1/({alpha}))",
  "paretoType2|({mu}) + ({lambda})*((1 - ({u}))^(-1/({alpha})) - 1)",
  ## bounded continuous
  "dbeta|ibetaInv({shape1}, {shape2}, {u})",
  "betaProportion|ibetaInv(({mu})*({kappa}), (1 - ({mu}))*({kappa}), {u})",
  "dunif|({min}) + (({max}) - ({min}))*({u})")

## built on first use rather than at load time: `.lotriDistTable` lives in
## `R/priors.R`, which the alphabetical collation loads after this file
.lotriEtaDistCache <- new.env(parent=emptyenv())

#' The eta distribution table, joined against the prior catalog
#'
#' @return data frame, cached after the first call
#' @noRd
#' @author Matthew L. Fidler
.lotriEtaDistTable <- function() {
  if (!is.null(.lotriEtaDistCache$tab)) return(.lotriEtaDistCache$tab)
  .l <- strsplit(.lotriEtaDistDefs, "|", fixed=TRUE)
  .nm <- vapply(.l, `[[`, character(1), 1L, USE.NAMES=FALSE)
  .q <- vapply(.l, `[[`, character(1), 2L, USE.NAMES=FALSE)
  .w <- match(.nm, .lotriDistTable$name)
  if (anyNA(.w)) {
    stop("eta distribution(s) not in the prior catalog: '", # nocov
         paste(.nm[is.na(.w)], collapse="', '"), "'", call.=FALSE) # nocov
  }
  .tab <- .lotriDistTable[.w, ]
  .tab$quantile <- .q
  rownames(.tab) <- NULL
  .lotriEtaDistCache$tab <- .tab
  .tab
}

##' Return the distributions that may be declared on a random effect
##'
##' These are the `lotriPriorDists()` families whose inverse CDF is
##' available, which is what a declared non-Gaussian eta needs: the
##' latent random effect stays standard normal and is mapped through
##' `Phi()` and then this quantile function.
##'
##' The extra `quantile` column is the 'rxode2' expression template for
##' that inverse CDF.  `{u}` stands for the uniform value and each
##' `{name}` for the distribution's parameter of that name; downstream
##' packages substitute the model's own expressions into it.
##'
##' @return data frame with the columns of [lotriPriorDists()] plus
##'   `quantile`
##'
##' @examples
##'
##' head(lotriEtaDists())
##'
##' @export
##' @seealso [lotriPriorDists()]
##' @author Matthew L. Fidler
lotriEtaDists <- function() {
  .lotriEtaDistTable()
}

#' Look up an eta distribution by any accepted spelling
#'
#' @param nm name used in the `dist()` statement
#' @return single row data frame, or NULL
#' @noRd
#' @author Matthew L. Fidler
.lotriEtaDistLookup <- function(nm) {
  .tab <- .lotriEtaDistTable()
  for (.col in c("name", "camelName", "stanName")) {
    .w <- which(.tab[[.col]] == nm)
    if (length(.w) == 1L) return(.tab[.w, ])
  }
  NULL
}

#' Is this expression a `dist(eta) ~ family(...)` line?
#'
#' @param x language object to test
#' @return TRUE when this declares an eta distribution
#' @noRd
#' @author Matthew L. Fidler
.lotriIsEtaDistLine <- function(x) {
  is.call(x) && length(x) == 3L &&
    identical(x[[1]], quote(`~`)) &&
    is.call(x[[2]]) && length(x[[2]][[1]]) == 1L &&
    as.character(x[[2]][[1]]) %in% c("dist", "etaDist")
}

#' Normalize a declared eta distribution call
#'
#' @param x language object of the distribution call, ie `dgamma(1, 2)`
#' @return list with `name`, `stanName`, `support`, `quantile`, `args`
#'   and `text` (the canonical deparsed declaration)
#' @noRd
#' @author Matthew L. Fidler
.lotriEtaDistNormalize <- function(x) {
  if (is.name(x)) {
    x <- as.call(list(x))
  }
  if (!is.call(x)) {
    stop("an eta distribution must be a distribution call like 'dgamma(1, 2)'",
         call.=FALSE)
  }
  .nm <- as.character(x[[1]])
  if (length(.nm) != 1L) {
    stop("unsupported eta distribution '", .deparse1(x), "'", call.=FALSE)
  }
  .w <- which(names(.lotriDistNotAliased) == .nm)
  if (length(.w) == 1L && nzchar(.lotriDistNotAliased[.w])) {
    stop("'", .nm, "' is not supported because it is parameterized differently than ",
         .lotriDistNotAliased[[.w]], "; use the Stan name and parameterization instead",
         call.=FALSE)
  }
  .dist <- .lotriEtaDistLookup(.nm)
  if (is.null(.dist)) {
    ## a real distribution, just not one that can be inverted
    .known <- .lotriPriorLookup(.nm)
    if (!is.null(.known)) {
      stop("'", .known$name, "' cannot be declared on a random effect: ",
           ifelse(.known$kind == "univariate",
                  "its quantile function is not available",
                  paste0("it is a ", .known$kind, " distribution, and a random ",
                         "effect declaration is univariate")),
           "; see 'lotriEtaDists()' for the ones that can", call.=FALSE)
    }
    stop("unknown eta distribution '", .nm, "'", .lotriPriorSuggest(.nm), call.=FALSE)
  }
  .parNames <- character(0)
  if (nzchar(.dist$parNames)) {
    .parNames <- strsplit(.dist$parNames, ",", fixed=TRUE)[[1]]
  }
  .args <- as.list(x)[-1]
  .args <- .lotriPriorMatchArgs(.args, .parNames, .dist$name, nReq=.dist$nReq)
  .txt <- paste0(.dist$name, "(",
                 paste(vapply(.args, .deparse1, character(1), USE.NAMES=FALSE),
                       collapse=", "), ")")
  list(name=.dist$name, stanName=.dist$stanName, support=.dist$support,
       quantile=.dist$quantile, args=.args, text=.txt)
}

#' Collect a `dist(eta) ~ family(...)` line
#'
#' Validated here so a syntax error is reported on the line it is on;
#' *resolved* against the matrix later, once any `rcm` re-ordering is
#' done, exactly like a `prior()`.
#'
#' @param x language object of the declaration line
#' @param env parsing environment
#' @return nothing, called for the side effect on `env$etaDists`
#' @noRd
#' @author Matthew L. Fidler
.fCallEtaDist <- function(x, env) {
  .fn <- as.character(x[[2]][[1]])
  .lhs <- as.list(x[[2]])[-1]
  if (length(.lhs) != 1L) {
    stop("'", .fn, "()' takes exactly one random effect name", call.=FALSE)
  }
  .nm <- .lhs[[1]]
  if (is.name(.nm)) {
    .nm <- as.character(.nm)
  } else if (!(is.character(.nm) && length(.nm) == 1L)) {
    stop("'", .fn, "()' takes a random effect name, not '",
         .deparse1(.lhs[[1]]), "'", call.=FALSE) # nolint
  }
  .rhs <- x[[3]]
  if (is.call(.rhs) && is.call(.rhs[[1]]) && length(.rhs[[1]]) == 3L &&
        (identical(.rhs[[1]][[1]], quote(`::`)) ||
           identical(.rhs[[1]][[1]], quote(`:::`)))) {
    .f2 <- as.character(.rhs[[1]][[3]])
    if (length(.f2) == 1L && !is.null(.lotriPriorLookup(.f2))) {
      stop("an eta distribution is not namespaced; write '", .f2,
           "(...)' rather than '", .deparse1(.rhs[[1]]), "(...)'", # nolint
           call.=FALSE)
    }
  }
  env$etaDists <- c(env$etaDists,
                    list(list(name=.nm, info=.lotriEtaDistNormalize(.rhs))))
  invisible()
}

#' Resolve the collected eta distributions against the matrices
#'
#' Matched by *name* so they are unaffected by any `rcm` re-ordering,
#' and stored as the `lotriEtaDists` attribute of the matrix the eta
#' belongs to, parallel to its dimnames -- the same shape and the same
#' rules as `lotriPriors`.
#'
#' @param ret matrix or list of matrices
#' @param etaDists list collected by `.fCallEtaDist()`
#' @return the amended `ret`
#' @noRd
#' @author Matthew L. Fidler
.lotriResolveEtaDists <- function(ret, etaDists) {
  if (length(etaDists) == 0L) return(ret)
  .isList <- !is.matrix(ret) && (inherits(ret, "list") || inherits(ret, "lotri"))
  .mats <- if (.isList) as.list(ret) else list(ret)
  .dst <- lapply(.mats, function(m) {
    if (!is.matrix(m)) return(character(0))
    rep(NA_character_, dim(m)[1])
  })
  .seen <- character(0)
  for (.d in etaDists) {
    .nm <- .d$name
    if (.nm %in% .seen) {
      stop("more than one distribution declared for '", .nm, "'", call.=FALSE)
    }
    .seen <- c(.seen, .nm)
    .found <- FALSE
    for (.k in seq_along(.mats)) {
      .m <- .mats[[.k]]
      if (!is.matrix(.m)) next
      .dn <- dimnames(.m)[[1]]
      if (is.null(.dn) || !(.nm %in% .dn)) next
      .at <- match(.nm, .dn)
      .same <- attr(.m, "lotriSame")
      if (!is.null(.same) && .same[.at] != 0L) {
        stop("'", .nm, "' repeats '", .dn[.at - .same[.at]],
             "' with 'same()', so it cannot declare its own distribution; ",
             "declare it on '", .dn[.at - .same[.at]], "'", call.=FALSE)
      }
      if (isTRUE(.lotriMatFixedDiag(.m, .nm))) {
        stop("'", .nm, "' is fixed, so it cannot declare a distribution; a ",
             "declared random effect is estimated on a unit-variance latent ",
             "scale", call.=FALSE)
      }
      .dst[[.k]][.at] <- .d$info$text
      .found <- TRUE
      break
    }
    if (!.found) {
      stop("distribution declared for unknown random effect: '", .nm, "'",
           call.=FALSE)
    }
  }
  for (.k in seq_along(.mats)) {
    if (all(is.na(.dst[[.k]]))) next
    .m <- .mats[[.k]]
    .lotriCheckEtaDistBlocks(.m, .dst[[.k]])
    attr(.m, "lotriEtaDists") <- .dst[[.k]]
    if (!inherits(.m, "lotriFix")) {
      class(.m) <- c("lotriFix", class(.m))
    }
    .mats[[.k]] <- .m
  }
  if (.isList) {
    .attr <- attributes(ret)
    ret <- .mats
    attributes(ret) <- .attr
    if (!inherits(ret, "lotriFix")) {
      class(ret) <- c("lotriFix", class(ret))
    }
  } else {
    ret <- .mats[[1]]
  }
  ret
}

#' Check the block a declared eta lives in is a correlation matrix
#'
#' A declared eta is standard normal on the latent scale, so the block
#' carries the Gaussian copula's CORRELATION, not a variance: every
#' diagonal element of a block containing one has to be exactly one, and
#' every off diagonal has to be a correlation.  nlmixr2 cannot fix single
#' components of an omega block, so this is stated by *estimating* the
#' diagonal at one rather than by fixing it.
#'
#' @param mat the matrix
#' @param dists the eta distribution vector, parallel to `dimnames(mat)[[1]]`
#' @return nothing, called for the error checking side effect
#' @noRd
#' @author Matthew L. Fidler
.lotriCheckEtaDistBlocks <- function(mat, dists) {
  .dn <- dimnames(mat)[[1]]
  .i <- 1L
  while (.i <= length(.dn)) {
    .idx <- .lotriBlockIndexes(mat, .i)
    if (any(!is.na(dists[.idx]))) {
      .bad <- .idx[mat[cbind(.idx, .idx)] != 1]
      if (length(.bad) > 0L) {
        stop("correlation/covariance between non-normal random effects needs ",
             "the diagonals estimated at one; '", .dn[.bad[1]], "' is ",
             format(mat[.bad[1], .bad[1]]),
             ifelse(length(.idx) == 1L,
                    ## a lone declared random effect implies `~ 1`, so the
                    ## shortest fix is to drop the line entirely
                    paste0(" -- drop the '", .dn[.idx[1]],
                           " ~ ...' line (a declared distribution implies '",
                           .dn[.idx[1]], " ~ 1')"),
                    paste0(" -- write '", paste(.dn[.idx], collapse=" + "),
                           " ~ c(", .lotriEtaDistCorEx(length(.idx)), ")'")),
             call.=FALSE)
      }
      if (length(.idx) > 1L) {
        .off <- mat[.idx, .idx, drop=FALSE]
        if (any(abs(.off[upper.tri(.off)]) >= 1)) {
          stop("the correlation between non-normal random effects '",
               paste(.dn[.idx], collapse="', '"),
               "' has to be between -1 and 1", call.=FALSE)
        }
        if (any(!is.na(dists[.idx])) &&
              min(eigen(.off, symmetric=TRUE, only.values=TRUE)$values) <= 0) {
          stop("the correlation block of '", paste(.dn[.idx], collapse="', '"),
               "' is not positive definite", call.=FALSE)
        }
      }
    }
    .i <- max(.idx) + 1L
  }
  invisible()
}

#' The `c(...)` a k x k correlation block would be written with
#'
#' Used only to make the "diagonals have to be one" message show the fix.
#'
#' @param k block dimension
#' @return character, ie `"1, 0.1, 1"`
#' @noRd
#' @author Matthew L. Fidler
.lotriEtaDistCorEx <- function(k) {
  .v <- character(0)
  for (.i in seq_len(k)) {
    .v <- c(.v, rep("0.1", .i - 1L), "1")
  }
  paste(.v, collapse=", ")
}

#' The unit variance a declared random effect implies
#'
#' A declared distribution says everything about the random effect's
#' marginal, so its "variance" carries no information: the latent scale is
#' standard normal by construction, which is why the block a declared
#' random effect lives in has to have a unit diagonal in the first place.
#' Making the user write `eta.cl ~ 1` alongside `dist(eta.cl) ~ ...` is
#' therefore asking them to repeat something the declaration already
#' fixed.
#'
#' So a `dist()` on a random effect that is not otherwise declared
#' implies `~ 1`, inserted just before the declaration so it lands at
#' whatever level that line is at.  A random effect that IS declared --
#' which is how a correlated block is written, since the correlation has
#' nowhere else to go -- is left exactly as written, and the unit
#' diagonal is then checked rather than assumed.
#'
#' @param x the `lotri({})` block expression
#' @return `x`, with an implied `name ~ 1` before each `dist()` line whose
#'   random effect is not declared anywhere in the block
#' @noRd
#' @author Matthew L. Fidler
.lotriEtaDistImplyVariance <- function(x) {
  if (!is.call(x)) return(x)
  .declared <- .lotriAllEtaLhsNames(x)
  .add <- function(y) {
    ## the name a `dist()`/`etaDist()` line names, or NULL
    if (!.lotriIsEtaDistLine(y)) return(NULL)
    .lhs <- as.list(y[[2]])[-1]
    if (length(.lhs) != 1L) return(NULL)
    .nm <- .lhs[[1]]
    if (is.name(.nm)) .nm <- as.character(.nm)
    if (!(is.character(.nm) && length(.nm) == 1L)) return(NULL)
    if (.nm %in% .declared) return(NULL)
    ## only the first `dist()` on a name implies it; a second is the
    ## "more than one distribution" error, not a second random effect
    .declared <<- c(.declared, .nm)
    str2lang(paste0(.nm, " ~ 1"))
  }
  if (identical(x[[1]], quote(`{`))) {
    .out <- list(quote(`{`))
    for (.i in seq_along(x)[-1]) {
      .imp <- .add(x[[.i]])
      if (!is.null(.imp)) .out[[length(.out) + 1L]] <- .imp
      .out[[length(.out) + 1L]] <- x[[.i]]
    }
    return(as.call(.out))
  }
  ## a single unbraced line, ie `lotri(dist(eta.cl) ~ dgamma(a, b))`
  .imp <- .add(x)
  if (is.null(.imp)) return(x)
  as.call(list(quote(`{`), .imp, x))
}
