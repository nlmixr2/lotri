#include <armadillo.hpp>
#include <cpp11.hpp>
#include <cpp11armadillo.hpp>
#include "cpp11/declarations.hpp"
#include "omeBlock.h"

extern "C" void _lotriOmega_mat(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);
extern "C" void _lotriOmega_mat_sqrt(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);
extern "C" void _lotriOmega_mat_log(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);

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
  SEXP retL = PROTECT(Rf_allocVector(VECSXP, 4));pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 4)); pro++;
  SEXP retI = PROTECT(Rf_allocVector(INTSXP, 1)); pro++;
  INTEGER(retI)[0] = diagXform;
  SEXP retV = PROTECT(Rf_allocVector(REALSXP, nlen)); pro++;
  double *retD = REAL(retV);
  int k = 0;
  for (int j = 0; j < n; ++j) {
    for (int i = 0; i <= j; ++i) {
      retD[k++] = cmat(i, j);
    }
  }
  SET_VECTOR_ELT(retL, 0, retV);
  SET_STRING_ELT(retN, 0, Rf_mkChar("theta"));
  SET_VECTOR_ELT(retL, 1, Rf_getAttrib(omeInS, R_DimNamesSymbol));
  SET_STRING_ELT(retN, 1, Rf_mkChar("dimnames"));
  SET_VECTOR_ELT(retL, 2, Rf_getAttrib(omeInS, R_DimSymbol));
  SET_STRING_ELT(retN, 2, Rf_mkChar("dim"));
  SET_VECTOR_ELT(retL, 3, retI);
  SET_STRING_ELT(retN, 3, Rf_mkChar("diagXform"));
  Rf_namesgets(retL, retN);
  Rf_classgets(retL, Rf_mkString("lotriOmegaBlock"));
  UNPROTECT(pro);
  return retL;
}

extern "C" SEXP _lotri_omegaBlockOp(SEXP omeBlock, SEXP opS) {

  int pro = 0;

  SEXP thetaS = VECTOR_ELT(omeBlock, 0);
  int n = Rf_length(thetaS);
  double *theta = REAL(thetaS);

  SEXP dimS = VECTOR_ELT(omeBlock, 2);
  int *dim = INTEGER(dimS);

  int *tn = INTEGER(opS);

  SEXP diagXformS = VECTOR_ELT(omeBlock, 3);
  int diagXform = INTEGER(diagXformS)[0];

  SEXP retS;

  int tn0 = tn[0];
  bool matrix = false;
  double *ret;
  switch (tn0) {
    // Matrix size
  case -1:
  case 0:
    matrix = true;
    retS = PROTECT(Rf_allocMatrix(REALSXP, dim[0], dim[1])); pro++;
    ret = REAL(retS);
    std::fill_n(ret, dim[0] * dim[1], 0.0);
    break;
  case -2:
    retS = PROTECT(Rf_allocVector(REALSXP, 1)); pro++;
    ret = REAL(retS);
    ret[0] = 0;
    break;
    // vector = theta number
  default:
    retS = PROTECT(Rf_allocVector(REALSXP, n)); pro++;
    ret = REAL(retS);
    std::fill_n(ret, n, 0.0);
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
    Rf_dimnamesgets(retS, VECTOR_ELT(omeBlock, 1));
  }
  UNPROTECT(pro);
  return retS;
}
