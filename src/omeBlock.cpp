#include <armadillo.hpp>
#include <cpp11.hpp>
#include <cpp11armadillo.hpp>
#include "cpp11/declarations.hpp"
#include "omeBlock.h"

extern "C" void _lotriOmega_mat(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);
extern "C" void _lotriOmega_mat_sqrt(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);
extern "C" void _lotriOmega_mat_log(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);

cpp11::environment newEnv() {
  cpp11::function newEnv(cpp11::package("lotri")[".newEnv"]);
  return newEnv();
}

extern "C" SEXP _lotri_omegaBlock(SEXP omeInS, SEXP diagXformS) {
  int diagXform = INTEGER(diagXformS)[0];
  cpp11::doubles_matrix<> omeIn = as_cpp<cpp11::doubles_matrix<>>(omeInS);
  arma::mat matin = as_Mat(omeIn);
  // Get the inverse of the matrix
  arma::mat imat;
  bool success = arma::inv(imat, matin);
  if (!success){
    imat = arma::pinv(matin);
  }
  arma::mat cmat;
  success = arma::chol(cmat, imat);
  if (!success) {
    cpp11::stop("can not cholesky decompose in 'lotri_omegaBlock'");
  }
  switch (diagXform) {
  case omegaXformSqrt:
    cmat.diag() = sqrt(cmat.diag());
    break;
  case omegaXformLog:
    cmat.diag() = log(cmat.diag());
    break;
  case omegaXformIdentity:
    break;
  }
  int pro=0;
  int n = cmat.n_rows;
  int nlen = n * (n + 1) / 2;
  SEXP retV = PROTECT(Rf_allocVector(REALSXP, nlen)); pro++;
  double *retD = REAL(retV);
  int k = 0;
  for (int j = 0; j < n; ++j) {
    for (int i = 0; i <= j; ++i) {
      retD[k++] = cmat(i, j);
    }
  }
  cpp11::environment env = newEnv();
  env["theta"] = retV;
  env["dimnames"] = Rf_getAttrib(omeInS, R_DimNamesSymbol);
  env["dim"] = Rf_getAttrib(omeInS, R_DimSymbol);
  env["diagXform"] = diagXform;

  SEXP ret = PROTECT(cpp11::as_sexp(env)); pro++;

  Rf_classgets(ret, Rf_mkString("lotriOmegaBlock"));

  UNPROTECT(pro);
  return ret;
}

SEXP _lotri_omegaBlockOpI(SEXP omeBlock, int op) {
  int pro = 0;
  int tn[1];
  tn[0] = op;

  cpp11::environment env = cpp11::as_cpp<cpp11::environment>(omeBlock);

  SEXP thetaS = env["theta"];
  int n = Rf_length(thetaS);
  double *theta = REAL(thetaS);

  SEXP dimS = env["dim"];
  int *dim = INTEGER(dimS);


  SEXP diagXformS = env["diagXform"];
  int diagXform = INTEGER(diagXformS)[0];

  SEXP retS;

  int tn0 = tn[0];
  bool matrix = false;
  double *ret;
  switch (tn0) {
  // Matrix size
  case -2:
    retS = PROTECT(Rf_allocVector(REALSXP, 1)); pro++;
    ret = REAL(retS);
    ret[0] = 0;
    break;
  // vector = theta number
  case -1:
  case 0:
  default:
    matrix = true;
    retS = PROTECT(Rf_allocMatrix(REALSXP, dim[0], dim[1])); pro++;
    ret = REAL(retS);
    std::fill_n(ret, dim[0] * dim[1], 0.0);
    break;
  }
  switch (diagXform) {
  case omegaXformSqrt:
    _lotriOmega_mat_sqrt(dim, theta, &n, tn, ret);
    break;

  case omegaXformLog:
    _lotriOmega_mat_log(dim, theta, &n, tn, ret);
    break;

  case omegaXformIdentity:
    _lotriOmega_mat(dim, theta, &n, tn, ret);
    break;
  }
  if (matrix) {
    Rf_dimnamesgets(retS, cpp11::as_sexp(env["dimnames"]));
  }
  UNPROTECT(pro);
  return retS;
}


arma::mat lotriCholOmega(arma::mat cholMat){
  // Only the cholesky is needed for the liklihood calculation
  // trimatu is faster, but it seems to have problems sometimes with certain BLAS combinations:
  // See https://github.com/nlmixrdevelopment/rxode2/issues/84
  // Only the cholesky is needed for the liklihood calculation
  // trimatu is faster, but it seems to have problems sometimes with certain BLAS combinations:
  // See https://github.com/nlmixrdevelopment/rxode2/issues/84
  arma::mat cholO;
  bool success;
  try {
    success = inv(cholO, trimatu(cholMat));
    if (success) return cholO;
    success = inv(cholO, cholMat);
    if (success) return cholO;
    cpp11::stop("can not invert in 'rxToCholOmega'");
  } catch (...) {
    success = inv(cholO, cholMat);
    if (success) return cholO;
    cpp11::stop("can not invert in 'rxToCholOmega'");
  }
  // should not get here.
  return cholO;
}



extern "C" SEXP _lotri_omegaBlockOp(SEXP omeBlock, SEXP opC) {
  try {
    cpp11::environment env = cpp11::as_cpp<cpp11::environment>(omeBlock);
    cpp11::strings op = cpp11::as_cpp<cpp11::strings>(opC);
    std::string op0 = cpp11::r_string(op[0]);
    if (env.exists(op0)) {
      return cpp11::as_sexp(env[op0]);
    }
    if (op0 == "cholOmegaInv") {
      env["cholOmegaInv"] = _lotri_omegaBlockOpI(omeBlock, 0);
      return cpp11::as_sexp(env["cholOmegaInv"]);
    } else if (op0 == "omegaInv") {
      env["omegaInv"] = _lotri_omegaBlockOpI(omeBlock, -1);
      return cpp11::as_sexp(env["omegaInv"]);
    } else if (op0 =="dOmegaInv") {
      cpp11::doubles theta = cpp11::as_cpp<cpp11::doubles>(env["theta"]);
      cpp11::writable::list dOmegaInv(theta.size());
      for (int i = 1; i < theta.size()+1; i++) {
        dOmegaInv[i-1] = _lotri_omegaBlockOpI(omeBlock, i);
      }
      env["dOmegaInv"] = dOmegaInv;
      return cpp11::as_sexp(env["dOmegaInv"]);
    } else if (op0 == "cholOmega1") {
      if (!env.exists("cholOmegaInv")) {
        env["cholOmegaInv"] = _lotri_omegaBlockOpI(omeBlock, 0);
      }
      arma::mat cholOmega1 = lotriCholOmega(as_Mat(cpp11::as_cpp<cpp11::doubles_matrix<>>(env["cholOmegaInv"])));
      SEXP cholOmega1S = PROTECT(Rf_allocMatrix(REALSXP, cholOmega1.n_rows, cholOmega1.n_cols));
      std::copy(cholOmega1.begin(), cholOmega1.end(), REAL(cholOmega1S));
      Rf_dimnamesgets(cholOmega1S, cpp11::as_sexp(env["dimnames"]));
      env["cholOmega1"] = cholOmega1S;
      UNPROTECT(1);
      return cpp11::as_sexp(env["cholOmega1"]);
    }
  } catch (...) {

  }
  return R_NilValue;
}
