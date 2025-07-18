omegaCreate <- function(dim, diag.xform = c("sqrt", "log", "identity")) {
  mat1 <- matrix(rep(1, dim * dim), dim)
  diag.xform <- match.arg(diag.xform)
  message("diagonal form: ", diag.xform)
  num <- as.vector(mat1[upper.tri(mat1, TRUE)])
  i <- 0
  num <- sapply(num, function(x) {
    if (x == 1) {
      i <<- i + 1
      return(i)
    } else {
      return(0)
    }
  })
  mat1[upper.tri(mat1, TRUE)] <- num - 1
  mat1[lower.tri(mat1)] <- t(mat1)[lower.tri(mat1)]
  d <- dim(mat1)[1]
  mat1 <- paste0("t", mat1)
  mat1[mat1 == "t-1"] <- "0"
  mat1 <- matrix(mat1, d)
  diags <- -2 - as.numeric(sapply(diag(mat1), function(x) {
    substring(x, 2)
  }))
  mat2 <- mat1
  if (diag.xform == "sqrt") {
    ## The diagonal elements are assumed to be estimated as sqrt
    diag(mat2) <- sprintf("%s=2", diag(mat2))
    diag(mat1) <- sprintf("%s^2", diag(mat1))
  } else if (diag.xform == "log") {
    ## The diagonal elements are assumed to be estimated as log
    diag(mat2) <- sprintf("%s=3", diag(mat2))
    diag(mat1) <- sprintf("exp(%s)", diag(mat1))
  } else {
    ## The diagonal elements are assumed to be estimated as identity
    diag(mat2) <- sprintf("%s=4", diag(mat2))
    diag(mat1) <- sprintf("(%s)", diag(mat1))
  }
  ## Cholesky is upper tri
  mat1[lower.tri(mat1)] <- "0"
  mat2[lower.tri(mat2)] <- "0"
  mat2[upper.tri(mat2)] <- sprintf("%s=5", mat2[upper.tri(mat2)])
  mat2 <- as.vector(mat2)
  mat2 <- mat2[mat2 != "0"]
  omat <- fmat <- mat1
  vars <- paste0("t", seq(0, i - 1))
  sdiag <- sprintf("(%s)^2", diag(omat))
  se.mat <- symengine::Matrix(omat)
  message("calculate symbolic inverse: t(chol.mat) %*% chol.mat ...", appendLF = FALSE)
  se.inv <- symengine::t(se.mat) %*% se.mat
  message("done")

  ## Then take the derivatives
  ## These are used in equations #28 and #47
  ##
  ##
  ## - Omega^-1 %*% dOmega %*% Omega^-1 = d(Omega^-1)
  ##
  ## In Equation #28
  ##
  ## Therefore:
  ##  1/2*t(eta) %*% (Omega^-1 %*% dOmega %*% Omega^-1)  %*% eta =
  ## -1/2*t(eta)*d(Omega^-1)*eta
  ##
  ## The second part is:
  ##
  ##  -1/2*tr(Omega^-1*dOmega) = +1/2*tr(Omega^-1 %*% Omega %*% d(Omega^-1)*Omega) or
  ## +1/2*tr(d(Omega^-1)*Omega);
  ##
  ## Omega needs to be inverted, but not symbolically.
  ##
  ## In fact the whole of dOmega does not need to be calculated,
  ## rather the diff(D_Omega^-1) where the D is the LDL^T
  ## factorization

  ## Equation #29 uses d(Omega^-1)
  ##
  ## Equation #47 uses
  ## -t(eta)*Omega^-1*(dOmega)*Omega^-1*d(eta)
  ## t(eta)*d(Omega^-1)*d(eta)
  ## Therefore NO symbolic derivatives of anything but d(Omega^-1) are required; These are below:
  cnt.i <- 0
  cnt <- function() {
    message(".", appendLF = FALSE)
    if (cnt.i %% 5 == 0) {
      message(cnt.i, apendLF = FALSE)
    }
    if (cnt.i %% 50 == 0) {
      message("", appendLF = TRUE)
    }
    cnt.i <<- cnt.i + 1
  }
  i <- 0
  j <- 0
  .m1 <- symengine::Matrix(sdiag)
  diag <- paste(lapply(diags, function(x) {
    .m <- symengine::D(.m1, symengine::S(paste0("t", -(x + 2))))
    .n <- dim(.m)[1]
    .str <- paste(sapply(seq(1, .n), function(d) {
      sprintf("      ret[%s] = %s;", d - 1, seC(.m[d, 1]))
    }), collapse = "\n")
    sprintf(
      "    %sif (*_tn == %s){\n%s\n    }", ifelse(-(x + 2) == 0, "", "else "), x - 1,
      .str
    )
  }), collapse = "\n")
  ## FIXME: this derivative expression is always the same. There
  ## should be a simpler way to express this...
  i <- 0
  omega0 <- sprintf(
    "    if (*_tn == 0){\n%s\n    }",
    paste(sapply(as.vector(omat), function(x) {
      ret <- sprintf("      ret[%s] = %s;", i, seC(x))
      i <<- i + 1
      return(ret)
    }), collapse = "\n")
  )
  i <- 0
  omega1 <- sprintf(
    "    else if (*_tn == -1){\n%s\n    }",
    paste(sapply(as.vector(se.inv), function(x) {
      cnt()
      ret <- sprintf("      ret[%s] = %s;", i, seC(x))
      i <<- i + 1
      return(ret)
    }), collapse = "\n")
  )
  omega1p <- paste(unlist(lapply(vars, function(x) {
    i <<- 0
    j <<- j + 1
    sprintf(
      "    else if (*_tn == %s){\n%s\n    }", j,
      paste(sapply(
        as.vector(symengine::D(se.inv, symengine::S(x))),
        function(x) {
          ret <- sprintf("      ret[%s] = %s;", i, seC(x))
          i <<- i + 1
          return(ret)
        }
      ), collapse = "\n")
    )
  })), collapse = "\n")
  ##

  mat2 <- sprintf(
    "if (*_tn== NA_INTEGER){\n%s\n    return;  \n}\n",
    paste(paste(gsub(rex::rex("t", capture(any_numbers), "="), "    ret[\\1]=", mat2), ";", sep = ""),
          collapse = "\n"
          )
  )
  matExpr <- ""
  vecExpr <- "    return;\n  }"
  src <- sprintf(
    "  %s\nif (*_tn == -2){\n    ret[0] = %s;\n    return;\n  }\n  else if (*_tn < %s || *_tn > %s){\n    ret[0] = NA_REAL;ret[1] = -1; //error(\"d(Omega^-1) derivative outside bounds\");\n  }\n  else if (*length_theta != %s){\n    ret[0] = NA_REAL;ret[1] = *length_theta;//error(\"requires vector with %s arguments\");\n  }\n%s\n%s\n%s",
    mat2, length(vars), min(diags) - 1, length(vars), length(vars), length(vars),
    paste0(matExpr, omega0), omega1, paste0(omega1p, "\n", vecExpr)
  )
  src <- strsplit(src, "\n")[[1]]
  reg <- rex::rex(any_spaces, "ret[", any_numbers, "]", any_spaces, "=", any_spaces, "0", any_spaces, ";")
  ## Take out the =0; expressions
  w <- which(regexpr(reg, src) != -1)
  if (length(w) > 0) {
    src <- paste(src[-w], collapse = "\n")
  } else {
    src <- paste(src, collapse = "\n")
  }
  message("done")
  fmat <- matrix(sapply(as.vector(fmat), function(x) {
    force(x)
    return(rxode2::rxFromSE(x))
  }), d)
  ret <- list(src, fmat)
  return(ret)
}


genOme <- function(mx=12) {
  omega.c <- devtools::package_file("src/omegaSqrt.c")
  file.out <- file(omega.c, "wb")
  writeLines(
    paste0(sprintf("//Generated from ::document() for %s dimensions\n#include <R.h>\n#include <Rdefines.h>\n#include <R_ext/Error.h>\n#include <Rmath.h>\nint _lotriOmega_matSize(void) {\n  return %d;\n}\n\nvoid _lotriOmega_mat_sqrt(int *dm, double *_t, int *length_theta, int  *_tn, double *ret){\n", mx, mx),
           paste(vapply(1:mx, function(x) {
             tmp <- omegaCreate(x, diag.xform="sqrt")[[1]]
             sprintf("%sif (*dm == %s) {\n%s\n",
                     ifelse(x==1, "", "else "),
                     x, tmp)
           }, character(1), USE.NAMES=FALSE), collapse=""), "\n  return;\n}"), file.out)
  close(file.out)

  omega.c <- devtools::package_file("src/omegaLog.c")
  file.out <- file(omega.c, "wb")
  writeLines(
    paste0(sprintf("//Generated from ::document() for %s dimensions\n#include <R.h>\n#include <Rdefines.h>\n#include <R_ext/Error.h>\n#include <Rmath.h>\nvoid _lotriOmega_mat_log(int *dm, double *_t, int *length_theta, int  *_tn, double *ret){\n", mx),
           paste(vapply(1:mx, function(x) {
             tmp <- omegaCreate(x, diag.xform="log")[[1]]
             sprintf("%sif (*dm == %s) {\n%s\n",
                     ifelse(x==1, "", "else "),
                     x, tmp)
           }, character(1), USE.NAMES=FALSE), collapse=""), "\n  return;\n}"), file.out)
  close(file.out)

  omega.c <- devtools::package_file("src/omegaIdentity.c")
  file.out <- file(omega.c, "wb")
  writeLines(
    paste0(sprintf("//Generated from ::document() for %s dimensions\n#include <R.h>\n#include <Rdefines.h>\n#include <R_ext/Error.h>\n#include <Rmath.h>\nvoid _lotriOmega_mat(int *dm, double *_t, int *length_theta, int  *_tn, double *ret){\n", mx),
           paste(vapply(1:mx, function(x) {
             tmp <- omegaCreate(x, diag.xform="identity")[[1]]
             sprintf("%sif (*dm == %s) {\n%s\n",
                     ifelse(x==1, "", "else "),
                     x, tmp)
           }, character(1), USE.NAMES=FALSE), collapse=""), "\n  return;\n}"), file.out)
  close(file.out)

  omega.c <- devtools::package_file("src/omegaLog.c")
  file.out <- file(omega.c, "wb")
  writeLines(
    paste0(sprintf("//Generated from ::document() for %s dimensions\n#include <R.h>\n#include <Rdefines.h>\n#include <R_ext/Error.h>\n#include <Rmath.h>\nvoid _lotriOmega_mat_log(int *dm, double *_t, int *length_theta, int  *_tn, double *ret){\n", mx),
           paste(vapply(1:mx, function(x) {
             tmp <- omegaCreate(x, diag.xform="log")[[1]]
             sprintf("%sif (*dm == %s) {\n%s\n",
                     ifelse(x==1, "", "else "),
                     x, tmp)
           }, character(1), USE.NAMES=FALSE), collapse=""), "\n  return;\n}"), file.out)
  close(file.out)
  ""
}

#' Get the number of built-in omega inverses
#'
#'
#' @return The dimension of the pre-compiled omega matrix translations
#' @author Matthew L. Fidler
#' @eval genOme()
#' @export
#' @examples
#' .nlmixr2omegaBuiltinSize()
.lotriOmegaBuiltinSize <- function() {
  .Call(`_lotriOmega_getBuiltinSize`)
}

#' Create a nlmixr2omega for use in focei
#'
#'
#' @param mat Matrix for conversion
#' @param diag.xform Form of the diagonal transformation
#' @return nlmixr2omega object
#' @author Matthew L. Fidler
#' @export
#'
#' @examples
#'
#' d <- 10
#' m <- matrix(rnorm(d^2), d, d)
#' mcov <- tcrossprod(m, m)
#' ome <-nlmixr2omega(mcov)
#'
lotriOmega <- function(mat, diag.xform = c("sqrt", "log", "identity")) {
  diag.xform <- as.integer(c("sqrt"=1L,
                             "log"=2L,
                             "identity"=0L)[match.arg(diag.xform)])
  .dn <- dimnames(mat)
  .in <- lotriMatInv(mat)
  .ptr <- .Call(`_lotriOmega_nlmixr2omegaNew`, .in, diag.xform)
  .ret <- list(.ptr, .dn)
  class(.ret) <- "lotriOmega"
  .ret
}

#' @export
`$.lotriOmega` <- function(obj, arg, exact = TRUE) {
  if (arg == "theta") {
    return(as.vector(getTheta(obj)))
  }
  if (arg == "omega") {
    return(getOmegaR(obj))
  }
  if (arg == "cholOmegaInv") {
    return(getCholOmegaInv(obj))
  }
  if (arg == "omegaInv") {
    return(getOmegaInv(obj))
  }
  ## FIXME
  ## if (arg == "dDomegaInv") {
  ##   return(getdDomegaInv(obj))
  ## }
  if (arg == "cholOmega1") {
    return(getCholOmega1(obj))
  }
  if (arg == "cholOmega") {
    return(getCholOmega(obj))
  }
  if (arg == "logDetOMGAinv5") {
    return(getLogDetOMGAinv5(obj))
  }
  if (arg == "tr28") {
    return(nlmixr2omega_tr28(obj))
  }
  NULL
}
