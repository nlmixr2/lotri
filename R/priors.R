## Prior distribution support for the lotri DSL
##
## The distributions here are the ones 'Stan' supports as sampling
## statements, so that a `prior()` specified in a `lotri({})`/`ini({})`
## block can be turned into a `target +=` statement downstream (in
## 'nlmixr2est').  This package only parses, validates and stores the
## prior; it does not generate any 'Stan' code.
##
## The table is intentionally data-driven; adding a distribution is a
## one line change to `.lotriDistDefs` below.

#' Definitions of the prior distributions understood by lotri
#'
#' Each entry is `rName|stanName|parNames|support|kind`
#'
#' - `rName` is the R spelling and is the canonical name whenever the R
#'   function uses the *same* parameterization as 'Stan'.  When there is
#'   no faithful R equivalent this is empty and the 'Stan' name becomes
#'   the canonical name.
#'
#' - `support` is used to check the prior against the declared
#'   lower/upper bounds of the parameter
#'
#' - `kind` determines what the prior may be attached to
#'
#' @noRd
#' @author Matthew L. Fidler
.lotriDistDefs <- c(
  ## unbounded continuous
  "dnorm|normal|mean,sd|real|univariate",
  "|std_normal||real|univariate",
  "|exp_mod_normal|mu,sigma,lambda|real|univariate",
  "|skew_normal|xi,omega,alpha|real|univariate",
  "|student_t|nu,mu,sigma|real|univariate",
  "dcauchy|cauchy|location,scale|real|univariate",
  "|double_exponential|mu,sigma|real|univariate",
  "dlogis|logistic|location,scale|real|univariate",
  "|gumbel|mu,beta|real|univariate",
  "|skew_double_exponential|mu,sigma,tau|real|univariate",
  ## positive continuous
  "dlnorm|lognormal|meanlog,sdlog|positive|univariate",
  "dchisq|chi_square|df|positive|univariate",
  "|inv_chi_square|nu|positive|univariate",
  "|scaled_inv_chi_square|nu,sigma|positive|univariate",
  "dexp|exponential|rate|positive|univariate",
  "dgamma|gamma|shape,rate|positive|univariate",
  "|inv_gamma|alpha,beta|positive|univariate",
  "dweibull|weibull|shape,scale|positive|univariate",
  "|frechet|alpha,sigma|positive|univariate",
  "|rayleigh|sigma|positive|univariate",
  "|wiener|alpha,tau,beta,delta|positive|univariate",
  "|pareto|y_min,alpha|positive|univariate",
  "|pareto_type_2|mu,lambda,alpha|nonneg|univariate",
  ## bounded continuous
  "dbeta|beta|shape1,shape2|unit|univariate",
  "|beta_proportion|mu,kappa|unit|univariate",
  "dunif|uniform|min,max|real|univariate",
  "|von_mises|mu,kappa|circular|univariate",
  ## simplex
  "|dirichlet|alpha|simplex|multivariate",
  ## correlation/covariance matrices
  "|lkj_corr|eta|corr|matrix",
  "|lkj_corr_cholesky|eta|corr|matrix",
  "|wishart|nu,Sigma|cov|matrix",
  "|inv_wishart|nu,Sigma|cov|matrix",
  "|wishart_cholesky|nu,L_S|cov|matrix",
  "|inv_wishart_cholesky|nu,L_S|cov|matrix",
  ## multivariate
  "|multi_normal|mu,Sigma|real|multivariate",
  "|multi_normal_prec|mu,Omega|real|multivariate",
  "|multi_normal_cholesky|mu,L|real|multivariate",
  "|multi_student_t|nu,mu,Sigma|real|multivariate",
  "|multi_student_t_cholesky|nu,mu,L|real|multivariate",
  ## discrete
  "|bernoulli|theta|int|discrete",
  "dbinom|binomial|size,prob|int|discrete",
  "|beta_binomial|N,alpha,beta|int|discrete",
  "|categorical|theta|int|discrete",
  "dpois|poisson|lambda|int|discrete",
  "|neg_binomial|alpha,beta|int|discrete",
  "|neg_binomial_2|mu,phi|int|discrete",
  "|multinomial|theta|int|discrete")

#' R functions that look like a Stan distribution but are parameterized
#' differently; aliasing them would silently change the prior
#'
#' @noRd
#' @author Matthew L. Fidler
.lotriDistNotAliased <- c(
  "dt"="student_t: R's 'dt()' is the standardized (or noncentral) t, while Stan's 'student_t(nu, mu, sigma)' is a location-scale t",
  "dnbinom"="neg_binomial_2: R's 'dnbinom()' uses size/prob, Stan's 'neg_binomial_2()' uses mu/phi",
  "dhyper"="hypergeometric: R and Stan order the arguments differently",
  "dwilcox"="")

.lotriDistTable <- local({
  .l <- strsplit(.lotriDistDefs, "|", fixed=TRUE)
  .get <- function(i, j) {
    vapply(.l, function(x) {
      if (length(x) < j) return("")
      x[j]
    }, character(1), USE.NAMES=FALSE)
  }
  .rName <- .get(seq_along(.l), 1)
  .stanName <- .get(seq_along(.l), 2)
  .parNames <- .get(seq_along(.l), 3)
  data.frame(rName=ifelse(nzchar(.rName), .rName, NA_character_),
             stanName=.stanName,
             name=ifelse(nzchar(.rName), .rName, .stanName),
             parNames=.parNames,
             nPar=vapply(.parNames, function(x) {
               if (!nzchar(x)) return(0L)
               length(strsplit(x, ",", fixed=TRUE)[[1]])
             }, integer(1), USE.NAMES=FALSE),
             support=.get(seq_along(.l), 4),
             kind=.get(seq_along(.l), 5),
             stringsAsFactors=FALSE)
})

##' Return the prior distributions supported by `lotri()`
##'
##' This is the table used to validate the `prior()` statements in a
##' `lotri({})`/`ini({})` block.  It is exported so that downstream
##' packages (like 'nlmixr2est') can translate a stored prior into the
##' 'Stan' spelling when generating a model.
##'
##' The `name` column is the canonical name that `lotri()` stores.  It
##' is the R spelling (`rName`) whenever R parameterizes the
##' distribution the same way 'Stan' does, and the 'Stan' spelling
##' (`stanName`) otherwise.
##'
##' @return data frame with columns `rName`, `stanName`, `name`,
##'   `parNames`, `nPar`, `support` and `kind`
##'
##' @examples
##'
##' head(lotriPriorDists())
##'
##' @export
##' @author Matthew L. Fidler
lotriPriorDists <- function() {
  .lotriDistTable
}

#' Look up a distribution by any accepted spelling
#'
#' @param nm character name used in the `prior()` statement
#' @return single row data frame of the distribution, or NULL
#' @noRd
#' @author Matthew L. Fidler
.lotriPriorLookup <- function(nm) {
  .w <- which(.lotriDistTable$name == nm)
  if (length(.w) == 1L) return(.lotriDistTable[.w, ])
  .w <- which(.lotriDistTable$stanName == nm)
  if (length(.w) == 1L) return(.lotriDistTable[.w, ])
  .w <- which(!is.na(.lotriDistTable$rName) & .lotriDistTable$rName == nm)
  if (length(.w) == 1L) return(.lotriDistTable[.w, ])
  NULL
}

#' Suggest the closest distribution name for an unknown one
#'
#' @param nm unknown distribution name
#' @return character suggestion suffix (possibly "")
#' @noRd
#' @author Matthew L. Fidler
.lotriPriorSuggest <- function(nm) {
  .all <- unique(c(.lotriDistTable$name, .lotriDistTable$stanName))
  .d <- utils::adist(nm, .all, ignore.case=TRUE)[1, ]
  .w <- which(.d == min(.d))
  if (length(.w) == 0L || min(.d) > 3) return("")
  paste0("; did you mean '", paste(.all[.w], collapse="' or '"), "'?")
}

#' Match the supplied prior arguments against the distribution parameters
#'
#' Both positional and named arguments are supported; the result is
#' always in canonical positional order.
#'
#' @param argList list of (possibly named) language objects
#' @param parNames character vector of the distribution's parameters
#' @param distName name used in the error messages
#' @return list of language objects in canonical order
#' @noRd
#' @author Matthew L. Fidler
.lotriPriorMatchArgs <- function(argList, parNames, distName) {
  .nArg <- length(argList)
  .nPar <- length(parNames)
  .argNames <- names(argList)
  if (is.null(.argNames)) .argNames <- rep("", .nArg)
  if (.nArg > .nPar) {
    stop("'", distName, "' takes ", .nPar, " argument(s) but ", .nArg, " were given",
         ifelse(.nPar == 0L, "", paste0(" (", paste(parNames, collapse=", "), ")")),
         call.=FALSE)
  }
  .out <- vector("list", .nPar)
  .used <- rep(FALSE, .nPar)
  for (.i in seq_len(.nArg)) {
    if (!nzchar(.argNames[.i])) next
    .w <- which(parNames == .argNames[.i])
    if (length(.w) != 1L) {
      stop("'", distName, "' has no argument '", .argNames[.i], "'",
           ifelse(.nPar == 0L, "",
                  paste0("; valid argument(s): ", paste(parNames, collapse=", "))),
           call.=FALSE)
    }
    if (.used[.w]) {
      stop("argument '", .argNames[.i], "' of '", distName, "' supplied more than once",
           call.=FALSE)
    }
    .out[[.w]] <- argList[[.i]]
    .used[.w] <- TRUE
  }
  .free <- which(!.used)
  .pos <- which(!nzchar(.argNames))
  if (length(.pos) > length(.free)) {
    stop("too many arguments given to '", distName, "'", call.=FALSE)
  }
  for (.j in seq_along(.pos)) {
    .out[[.free[.j]]] <- argList[[.pos[.j]]]
    .used[.free[.j]] <- TRUE
  }
  if (!all(.used)) {
    stop("'", distName, "' is missing argument(s): ",
         paste(parNames[!.used], collapse=", "), call.=FALSE)
  }
  .out
}

#' Normalize a prior distribution call
#'
#' @param x language object of the distribution call, ie `dnorm(0, 10)`
#' @return list with `name` (canonical), `stanName`, `support`, `kind`
#'   and `text` (the canonical deparsed prior)
#' @noRd
#' @author Matthew L. Fidler
.lotriPriorNormalize <- function(x) {
  if (is.name(x)) {
    ## allow `std_normal` without parentheses
    x <- as.call(list(x))
  }
  if (!is.call(x)) {
    stop("a prior must be a distribution call like 'dnorm(0, 10)'", call.=FALSE)
  }
  .nm <- as.character(x[[1]])
  if (length(.nm) != 1L) {
    stop("unsupported prior distribution '", .deparse1(x), "'", call.=FALSE)
  }
  .w <- which(names(.lotriDistNotAliased) == .nm)
  if (length(.w) == 1L && nzchar(.lotriDistNotAliased[.w])) {
    stop("'", .nm, "' is not supported because it is parameterized differently than ",
         .lotriDistNotAliased[[.w]], "; use the Stan name and parameterization instead",
         call.=FALSE)
  }
  .dist <- .lotriPriorLookup(.nm)
  if (is.null(.dist)) {
    stop("unknown prior distribution '", .nm, "'", .lotriPriorSuggest(.nm), call.=FALSE)
  }
  .parNames <- character(0)
  if (nzchar(.dist$parNames)) {
    .parNames <- strsplit(.dist$parNames, ",", fixed=TRUE)[[1]]
  }
  .args <- as.list(x)[-1]
  .args <- .lotriPriorMatchArgs(.args, .parNames, .dist$name)
  .txt <- paste0(.dist$name, "(",
                 paste(vapply(.args, .deparse1, character(1), USE.NAMES=FALSE),
                       collapse=", "), ")")
  list(name=.dist$name, stanName=.dist$stanName, support=.dist$support,
       kind=.dist$kind, text=.txt)
}

#' Check a normalized prior against its target
#'
#' @param info result of `.lotriPriorNormalize()`
#' @param names character vector of parameter name(s) the prior is on
#' @param lower numeric lower bound(s); may be NA when unknown
#' @param upper numeric upper bound(s); may be NA when unknown
#' @param isBlock is the target a covariance block (length > 1)
#' @return nothing, called for the error checking side effect
#' @noRd
#' @author Matthew L. Fidler
.lotriPriorCheckTarget <- function(info, names, lower=NA_real_, upper=NA_real_,
                                   isBlock=FALSE) {
  .n <- length(names)
  .what <- paste0("'", paste(names, collapse=", "), "'")
  if (info$kind == "matrix") {
    ## lkj_corr()/wishart() and friends are priors on a matrix, so they
    ## need an actual covariance block
    if (!isBlock || .n < 2L) {
      stop("prior '", info$name, "' applies to a covariance block, but ", .what,
           " is a single parameter", call.=FALSE)
    }
  } else if (info$kind == "multivariate") {
    ## multi_normal() and friends are priors on a vector of parameters,
    ## so they work on a covariance block *or* a group of estimates
    if (.n < 2L) {
      stop("prior '", info$name, "' applies to more than one parameter, but ",
           .what, " is a single parameter", call.=FALSE)
    }
  } else if (.n > 1L) {
    stop("prior '", info$name, "' is univariate and cannot be applied to the block ",
         .what, call.=FALSE)
  }
  if (info$kind == "univariate") {
    .lower <- suppressWarnings(min(lower, na.rm=TRUE))
    .upper <- suppressWarnings(max(upper, na.rm=TRUE))
    if (is.finite(.lower) || is.finite(.upper)) {
      if (info$support %in% c("positive", "nonneg") && is.finite(.lower) && .lower < 0) {
        stop("prior '", info$name, "' has positive support but ", .what,
             " has a lower bound of ", .lower, call.=FALSE)
      }
      if (info$support == "unit" &&
            ((is.finite(.lower) && .lower < 0) || (is.finite(.upper) && .upper > 1))) {
        stop("prior '", info$name, "' has support on [0, 1] but ", .what,
             " is bounded outside of it", call.=FALSE)
      }
    }
  }
  invisible()
}
