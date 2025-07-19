#define STRICT_R_HEADER
#define ARMA_WARN_LEVEL 1
#define ARMA_DONT_USE_OPENMP // Known to cause speed problems

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <Rmath.h>
#include <stdlib.h>

#include <armadillo.hpp>
#include <cpp11.hpp>
#include <cpp11armadillo.hpp>
#include "cpp11/declarations.hpp"

using namespace arma;

#include "../inst/include/lotriOmegaArma.h"
#include "omegaR.h"
#include "omega.h"
#define as_vec as_Col

void _lotriOmegaAssignTheta(_lotriOmega_ind_omega *ome, arma::vec theta) {
  arma::vec cur = theta;
  ome->theta = cur;
  // sum_{k=1}^{n} k = n*(n+1)/2
  int d0 = theta.size();
  ome->dim = 0.5*sqrt(1.0 + d0 * 8.0) - 0.5;
  // n^2 + n = d0*2
  ome->cholOmegaInvBool = false;
  ome->omegaInvBool = false;
  ome->dOmegaInvBool = false;
  ome->dDomegaInvBool = false;
  ome->cholOmega1Bool = false;
  ome->omegaBool = false;
  ome->cholOmegaBool = false;
  ome->logDetOMGAinv5Bool = false;
  ome->tr28Bool = false;
  ome->omega47Bool = false;
}

arma::mat lotriOmega_cholOmegaInv(_lotriOmega_ind_omega *ome) {
  if (ome->cholOmegaInvBool && ome->cholOmegaInvMat.n_elem != 0) return ome->cholOmegaInvMat;
  int ts = ome->theta.size();
  int tn  = 0;
  arma::mat ret(ome->dim, ome->dim, arma::fill::zeros);
  ome->cFun(&(ome->dim), ome->theta.memptr(), &ts, &tn, ret.memptr());
  ome->cholOmegaInvMat = ret;
  ome->cholOmegaInvBool = true;
  return ome->cholOmegaInvMat;
}


arma::mat lotriOmega_omegaInv(_lotriOmega_ind_omega *ome) {
 if (ome->omegaInvBool && ome->omegaInvMat.n_elem != 0) return ome->omegaInvMat;
 int ts = ome->theta.size();
 int tn  = -1;
 arma::mat ret(ome->dim, ome->dim, arma::fill::zeros);
 ome->cFun(&(ome->dim), ome->theta.memptr(), &ts, &tn, ret.memptr());
 ome->omegaInvMat = ret;
 ome->omegaInvBool = true;
 return ome->omegaInvMat;
}

arma::cube lotriOmega_dOmegaInv(_lotriOmega_ind_omega *ome) {
  if (ome->dDomegaInvBool && ome->dOmegaInvCube.n_elem != 0) return ome->dOmegaInvCube;
  int ts = ome->theta.size();
  arma::cube ret(ome->dim, ome->dim, ts, arma::fill::zeros);
  int tn;
  for (int i = 0; i < ts; ++i) {
    tn = i+1;
    ome->cFun(&(ome->dim), ome->theta.memptr(), &ts, &tn, ret.slice(i).memptr());
  }
  ome->dOmegaInvCube = ret;
  ome->dDomegaInvBool = true;
  return ome->dOmegaInvCube;
}

arma::mat lotriOmega_dDomegaInv(_lotriOmega_ind_omega *ome) {
  if (ome->dDomegaInvBool && ome->dDomegaInvMat.n_elem != 0) return (ome->dDomegaInvMat);
  int ts = ome->theta.size();
  // dim  x ntheta ; "d.D.omegaInv"
  arma::mat ret(ome->dim, ts, arma::fill::zeros);
  int tn;
  for (int i = 0; i < ts; ++i) {
    tn = -2 - (i+1);
    ome->cFun(&(ome->dim), ome->theta.memptr(), &ts, &tn, ret.memptr() + i*ome->dim);
  }
  ome->dDomegaInvMat = ret;
  ome->dDomegaInvBool = true;
  return ome->dDomegaInvMat;
}

arma::mat rxToCholOmega(arma::mat cholMat){
  arma::mat cholO;
  bool success = false;
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

arma::mat lotriOmega_cholOmega1(_lotriOmega_ind_omega *ome) {
  if (ome->cholOmega1Bool && ome->cholOmega1Mat.n_elem != 0) return ome->cholOmega1Mat;
  arma::mat coi =  lotriOmega_cholOmegaInv(ome);
  coi = rxToCholOmega(coi);
  ome->cholOmega1Bool = true;
  ome->cholOmega1Mat = coi;
  return ome->cholOmega1Mat;
}

arma::mat lotriOmega_omega(_lotriOmega_ind_omega *ome) {
  if (ome->omegaBool && ome->omegaMat.n_elem != 0) return ome->omegaMat;
  arma::mat u1 = lotriOmega_cholOmega1(ome);
  arma::mat omega = u1*trans(u1);
  ome->omegaBool = true;
  ome->omegaMat = omega;
  return ome->omegaMat;
}

arma::mat lotriOmega_cholOmega(_lotriOmega_ind_omega *ome) {
  if (ome->cholOmegaBool) return ome->cholOmegaMat;
  ome->cholOmegaBool = true;
  ome->cholOmegaMat = chol(lotriOmega_omega(ome));
  return ome->cholOmegaMat;
}

double lotriOmega_logDetOMGAinv5(_lotriOmega_ind_omega *ome) {
  if (ome->logDetOMGAinv5Bool) return ome->logDetOMGAinv5double;
  arma::mat coi =  lotriOmega_cholOmegaInv(ome);
  arma::vec diag = coi.diag();
  arma::vec ldiag = log(diag);
  ome->logDetOMGAinv5double = sum(ldiag);
  ome->logDetOMGAinv5Bool = true;
  return ome->logDetOMGAinv5double;
}

arma::vec nlmixr2omega_tr28(_lotriOmega_ind_omega *ome) {
  // 1/2*tr(d(Omega^-1)*Omega);
  if (ome->tr28Bool) return ome->tr28vec;
  unsigned int ts = ome->theta.size();
  arma::mat omega = lotriOmega_omega(ome);
  arma::cube dOmegaInv = lotriOmega_dOmegaInv(ome);
  arma::vec tr28(ts);
  for (unsigned int i = ts; i--;){
    arma::mat cur = (dOmegaInv.slice(i) * omega);
    tr28[i] =0.5*sum(cur.diag());
  }
  ome->tr28vec = tr28;
  ome->tr28Bool = true;
  return ome->tr28vec;
}

arma::cube lotriOmega_omega47(_lotriOmega_ind_omega *ome) {
  if (ome->omega47Bool) return ome->omega47Cube;
  arma::mat cholO =  lotriOmega_cholOmegaInv(ome);
  arma::cube dOmegaInv = lotriOmega_dOmegaInv(ome);
  arma::mat cEta = zeros(ome->dim,1);
  arma::mat c;
  unsigned int ts = ome->theta.size();
  arma::cube ret(ome->dim, ome->dim, ts, arma::fill::none);
  for (unsigned int i = ts; i--;){
    c = dOmegaInv.slice(i);
    arma::mat prodI(ome->dim, ome->dim, arma::fill::none);
    for (unsigned int j = ome->dim; j--;){
      cEta(j,0) = 1;
      prodI.col(j) = c*cEta;
      cEta(j,0) = 0;
    }
    ret.slice(i) = prodI;
  }
  ome->omega47Cube = ret;
  ome->omega47Bool = true;
  return ome->omega47Cube;
}


// Inverts the matrix
arma::mat lotriOmega_inv(arma::mat &smatrix) {
  // Invert matrix using RcppArmadillo.
  arma::mat imat;
  bool success;
  success = arma::inv(imat, smatrix);
  if (!success){
    imat = arma::pinv(smatrix);
    //REprintf(_("matrix seems singular; Using pseudo-inverse\n"));
  }
  return imat;
}

// Get the cholesky decomposition of the inverse
arma::mat lotriOmega_cholInv(arma::mat &mat) {
  return arma::chol(lotriOmega_inv(mat));
}

void lotriOmega_iniOmeStruct(_lotriOmega_ind_omega *ome,
                               arma::mat &mat, int diagXform) {
  arma::mat in =  lotriOmega_cholInv(mat);
  switch (diagXform) {
  case lotriOmega_sqrt:
    in.diag() = sqrt(in.diag());
    ome->cFun = _lotriOmega_mat_sqrt;
    break;
  case lotriOmega_log:
    in.diag() = log(in.diag());
    ome->cFun = _lotriOmega_mat_log;
  default:
    ome->cFun = _lotriOmega_mat;
    break;
  }
  arma::vec theta = in(trimatu_ind(size(in)));
  _lotriOmegaAssignTheta(ome, theta);
}

arma::vec _lotriOmega_full_getTheta_(_lotriOmega_full_omega *fome) {
  if (fome->nomes == 0) {
    arma::vec ret;
    return ret;
  } else if (fome->nomes == 1) {
    return fome->omes[0].theta;
  } else {
    arma::vec theta = fome->omes[0].theta;
    for (int i = 1; i < fome->nomes; ++i) {
      theta = join_cols(theta, fome->omes[i].theta);
    }
    return theta;
  }
}

arma::vec lotriOmegaNewVec(_lotriOmega_full_omega *fullPtr, int diagXform) {
  arma::vec ret(4);
  ret[0] = fullPtr->nomes;
  ret[1] = fullPtr->nTotTheta;
  ret[2] = fullPtr->nTotDim;
  ret[3] = diagXform;
  for (int i = 0; i < fullPtr->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fullPtr->omes[i]);
    arma::vec n(1);
    n[0] = ome->dim;
    ret= join_cols(ret, n);
  }
  arma::vec theta = _lotriOmega_full_getTheta_(fullPtr);
  free(fullPtr->omes);
  fullPtr->omes = NULL;
  //arma::vec theta = _nlmixr2omega_full_getTheta_(fullPtr);
  return join_cols(ret, theta);
}


arma::vec lotriOmegaNew(cpp11::list omeList, int diagXform) {
  _lotriOmega_full_omega full;
  _lotriOmega_full_omega *fullPtr = &full;
  fullPtr->nomes = omeList.size();
  if (fullPtr->omes != NULL) free(fullPtr->omes);
  fullPtr->omes = (_lotriOmega_ind_omega*)malloc(fullPtr->nomes*sizeof(_lotriOmega_ind_omega));
  fullPtr->nTotTheta = 0;
  fullPtr->nTotDim = 0;
  for (int i = 0; i < fullPtr->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fullPtr->omes[i]);
    cpp11::doubles_matrix<> curD = as_cpp<cpp11::doubles_matrix<>>(omeList[i]);
    arma::mat cur = as_mat(curD);
    lotriOmega_iniOmeStruct(ome, cur, diagXform);
    fullPtr->nTotTheta += ome->theta.size();
    fullPtr->nTotDim += ome->dim;
  }
  return lotriOmegaNewVec(fullPtr, diagXform);
}

extern "C" SEXP _lotri_lotriOmegaNew(SEXP omeListS, SEXP diagXformS) {
  try {
    int diagXform = INTEGER(diagXformS)[0];
    cpp11::list omeList = as_cpp<cpp11::list>(omeListS);
    cpp11::doubles ret = as_doubles(lotriOmegaNew(omeList, diagXform));
    return cpp11::as_sexp(ret);
  } catch (...) {
  }
  return R_NilValue;
}

_lotriOmega_full_omega lotriOmega_full_Create(arma::vec in) {
  _lotriOmega_full_omega full;
  _lotriOmega_full_omega *fullPtr = &full;
  double *ptr = in.memptr();
  fullPtr->nomes = (int)(in[0]);
  fullPtr->nTotTheta = (int)(in[1]);
  fullPtr->nTotDim = (int)(in[2]);
  int diagXform = (int)(in[3]);
  ptr += 4;
  fullPtr->omes = (_lotriOmega_ind_omega*)malloc(fullPtr->nomes*sizeof(_lotriOmega_ind_omega));
  for (int i = 0; i < fullPtr->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fullPtr->omes[i]);
    ome->cholOmegaInvBool = false;
    ome->omegaInvBool = false;
    ome->dOmegaInvBool = false;
    ome->dDomegaInvBool = false;
    ome->cholOmega1Bool = false;
    ome->omegaBool = false;
    ome->cholOmegaBool = false;
    ome->logDetOMGAinv5Bool=false;
    ome->tr28Bool = false;
    ome->omega47Bool = false;
    switch (diagXform) {
    case lotriOmega_sqrt:
      ome->cFun = _lotriOmega_mat_sqrt;
      break;
    case lotriOmega_log:
      ome->cFun = _lotriOmega_mat_log;
    default:
      ome->cFun = _lotriOmega_mat;
      break;
    }
    ome->dim = ptr[0];
    ptr++;
  }
  for (int i = 0; i < fullPtr->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fullPtr->omes[i]);
    int ntheta = 0.5*(ome->dim)*(ome->dim+1.0);
    arma::vec theta(ntheta);
    std::copy(ptr, ptr+ntheta, theta.begin());
    ptr += ntheta;
    _lotriOmegaAssignTheta(ome, theta);
  }
  return full;
}

_lotriOmega_full_omega omegaFromRgetFullOmegaFromSexp(SEXP inSEXP) {
  SEXP cur = PROTECT(VECTOR_ELT(inSEXP, 0));
  try {
    double *ptr = REAL(cur);
    arma::vec in(ptr, Rf_length(cur), false, true);
    UNPROTECT(1);
    return lotriOmega_full_Create(in);
  } catch(...) {
    UNPROTECT(1);
    Rf_error("Error in omegaFromRgetFullOmegaFromSexp");
  }
}

int omegaFromRgetDiagXfrom(SEXP inSEXP) {
  return INTEGER(VECTOR_ELT(inSEXP, 0))[3];
}

cpp11::doubles getTheta(SEXP inSEXP) {
  _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
  return as_doubles(_lotriOmega_full_getTheta_(&p));
}

extern "C" SEXP _lotri_getTheta(SEXP inSEXP) {
  try {
    cpp11::doubles ret = getTheta(inSEXP);
    return cpp11::as_sexp(ret);
  } catch (...) {
    Rf_error("Error in _lotri_getTheta");
  }
  return R_NilValue;
}


void _lotriOmega_full_setTheta_(_lotriOmega_full_omega *fome,
                                arma::vec theta) {
  if ((unsigned int)fome->nTotTheta != theta.size()) {
    Rf_error("incompatible size with this omega structure");
  }
  double *ptr = theta.memptr();
  for (int i = 0; i < fome->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fome->omes[i]);
    arma::vec curTheta = arma::vec(ptr, ome->theta.size());
    _lotriOmegaAssignTheta(ome, curTheta);
    ptr += curTheta.size();
  }
}

SEXP setTheta(SEXP inSEXP, arma::vec theta) {
  _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
  _lotriOmega_full_setTheta_(&p, theta);
  int pro=0;
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 2)); pro++;
  arma::vec onv;
  try {
    onv = lotriOmegaNewVec(&p, omegaFromRgetDiagXfrom(inSEXP));
  } catch (...) {
    UNPROTECT(pro);
    Rf_error("Error in setTheta");
    return R_NilValue;
  }
  SEXP nv =  PROTECT(Rf_allocVector(REALSXP, onv.size())); pro++;
  double *ptr = REAL(nv);
  std::copy(onv.begin(), onv.end(), ptr);
  SET_VECTOR_ELT(ret, 0, nv);
  SET_VECTOR_ELT(ret, 1, VECTOR_ELT(inSEXP, 1));
  SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(cls, 0, Rf_mkChar("lotriOmega"));
  Rf_classgets(ret, cls);
  UNPROTECT(pro);
  return ret;
}


extern "C" SEXP _lotri_setTheta(SEXP inSEXP, SEXP thetaS) {
  arma::vec theta = arma::vec(REAL(thetaS), Rf_length(thetaS), false, true);
  return setTheta(inSEXP, theta);
}

arma::mat _lotriOmega_full_cholOmegaInv(_lotriOmega_full_omega *fome) {
  arma::mat ret(fome->nTotDim, fome->nTotDim, arma::fill::zeros);
  int curBlock = 0;
  for (int i = 0; i < fome->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fome->omes[i]);
    int curDim = ome->dim;
    ret.submat(curBlock, curBlock,
               curBlock+curDim-1, curBlock+curDim-1) =
      lotriOmega_cholOmegaInv(ome);
    curBlock += curDim;
  }
  return ret;
}

extern "C" SEXP _lotri_getCholOmegaInv(SEXP inSEXP) {
  _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
  arma::mat cholInv = _lotriOmega_full_cholOmegaInv(&p);
  int pro = 0;
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, cholInv.n_elem)); pro++;
  SEXP dim = PROTECT(Rf_allocVector(INTSXP, 2)); pro++;
  int *dimPtr = INTEGER(dim);
  dimPtr[0] = cholInv.n_rows;
  dimPtr[1] = cholInv.n_cols;
  Rf_dimgets(ret, dim);
  Rf_dimnamesgets(ret, VECTOR_ELT(inSEXP, 1));
  UNPROTECT(pro);
  return ret;
}


arma::mat _lotriOmega_full_omegaInv(_lotriOmega_full_omega *fome) {
  arma::mat ret(fome->nTotDim, fome->nTotDim, arma::fill::zeros);
  int curBlock = 0;
  for (int i = 0; i < fome->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fome->omes[i]);
    int curDim = ome->dim;
    ret.submat(curBlock, curBlock,
               curBlock+curDim-1, curBlock+curDim-1) =
      lotriOmega_omegaInv(ome);
    curBlock += curDim;
  }
  return ret;
}

extern "C" SEXP _lotri_getOmegaInv(SEXP inSEXP) {
  _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
  arma::mat retm = _lotriOmega_full_omegaInv(&p);
  int pro = 0;
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, retm.n_elem)); pro++;
  SEXP dim = PROTECT(Rf_allocVector(INTSXP, 2)); pro++;
  int *dimPtr = INTEGER(dim);
  dimPtr[0] = retm.n_rows;
  dimPtr[1] = retm.n_cols;
  Rf_dimgets(ret, dim);
  Rf_dimnamesgets(ret, VECTOR_ELT(inSEXP, 1));
  UNPROTECT(pro);
  return ret;
}

arma::mat _lotriOmega_full_dDomegaInv(_lotriOmega_full_omega *fome) {
  arma::mat ret(fome->nTotDim, fome->nTotDim, arma::fill::zeros);
  int curBlock = 0;
  for (int i = 0; i < fome->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fome->omes[i]);
    int curDim = ome->dim;
    ret.submat(curBlock, curBlock,
               curBlock+curDim-1, curBlock+curDim-1) =
      lotriOmega_dDomegaInv(ome);
    curBlock += curDim;
  }
  return ret;
}

extern "C" SEXP _lotri_getdDomegaInv(SEXP inSEXP) {
  _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
  arma::mat retm = _lotriOmega_full_dDomegaInv(&p);
  int pro = 0;
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, retm.n_elem)); pro++;
  SEXP dim = PROTECT(Rf_allocVector(INTSXP, 2)); pro++;
  int *dimPtr = INTEGER(dim);
  dimPtr[0] = retm.n_rows;
  dimPtr[1] = retm.n_cols;
  Rf_dimgets(ret, dim);
  UNPROTECT(pro);
  return ret;
}

arma::mat _lotriOmega_full_cholOmega1(_lotriOmega_full_omega *fome) {
  arma::mat ret(fome->nTotDim, fome->nTotDim, arma::fill::zeros);
  int curBlock = 0;
  for (int i = 0; i < fome->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fome->omes[i]);
    int curDim = ome->dim;
    ret.submat(curBlock, curBlock,
               curBlock+curDim-1, curBlock+curDim-1) =
      lotriOmega_cholOmega1(ome);
    curBlock += curDim;
  }
  return ret;
}

extern "C" SEXP _lotri_getCholOmega1(SEXP inSEXP) {
  _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
  arma::mat retm = _lotriOmega_full_cholOmega1(&p);
  int pro = 0;
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, retm.n_elem)); pro++;
  SEXP dim = PROTECT(Rf_allocVector(INTSXP, 2)); pro++;
  int *dimPtr = INTEGER(dim);
  dimPtr[0] = retm.n_rows;
  dimPtr[1] = retm.n_cols;
  Rf_dimgets(ret, dim);
  Rf_dimnamesgets(ret, VECTOR_ELT(inSEXP, 1));
  UNPROTECT(pro);
  return ret;
}

arma::mat _lotriOmega_full_omegaR(_lotriOmega_full_omega *fome) {
  arma::mat ret(fome->nTotDim, fome->nTotDim, arma::fill::zeros);
  int curBlock = 0;
  for (int i = 0; i < fome->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fome->omes[i]);
    int curDim = ome->dim;
    arma::mat cur = lotriOmega_omega(ome);
    ret.submat(curBlock, curBlock,
               curBlock+curDim-1, curBlock+curDim-1) = cur;
    curBlock += curDim;
  }
  return ret;
}

extern "C" SEXP _lotri_getOmegaR(SEXP inSEXP) {
  _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
  arma::mat retm = _lotriOmega_full_omegaR(&p);
  int pro = 0;
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, retm.n_elem)); pro++;
  SEXP dim = PROTECT(Rf_allocVector(INTSXP, 2)); pro++;
  int *dimPtr = INTEGER(dim);
  dimPtr[0] = retm.n_rows;
  dimPtr[1] = retm.n_cols;
  Rf_dimgets(ret, dim);
  Rf_dimnamesgets(ret, VECTOR_ELT(inSEXP, 1));
  UNPROTECT(pro);
  return ret;
}

arma::mat _lotriOmega_full_cholOmega(_lotriOmega_full_omega *fome) {
  arma::mat ret(fome->nTotDim, fome->nTotDim, arma::fill::zeros);
  int curBlock = 0;
  for (int i = 0; i < fome->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fome->omes[i]);
    int curDim = ome->dim;
    ret.submat(curBlock, curBlock,
               curBlock+curDim-1, curBlock+curDim-1) =
      lotriOmega_cholOmega(ome);
    curBlock += curDim;
  }
  return ret;
}

extern "C" SEXP _lotri_getCholOmega(SEXP inSEXP) {
 _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
 arma::mat retm = _lotriOmega_full_cholOmega(&p);
 int pro = 0;
 SEXP ret = PROTECT(Rf_allocVector(REALSXP, retm.n_elem)); pro++;
 SEXP dim = PROTECT(Rf_allocVector(INTSXP, 2)); pro++;
 int *dimPtr = INTEGER(dim);
 dimPtr[0] = retm.n_rows;
 dimPtr[1] = retm.n_cols;
 Rf_dimgets(ret, dim);
 Rf_dimnamesgets(ret, VECTOR_ELT(inSEXP, 1));
 UNPROTECT(pro);
 return ret;
}

double _lotriOmega_full_logDetOMGAinv5(_lotriOmega_full_omega *fome) {
  double ret = 0.0;
  for (int i = 0; i < fome->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fome->omes[i]);
    ret += lotriOmega_logDetOMGAinv5(ome);
  }
  return ret;
}

extern "C" SEXP _lotri_getLogDetOMGAinv5(SEXP inSEXP) {
  _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(ret)[0] = _lotriOmega_full_logDetOMGAinv5(&p);
  UNPROTECT(1);
  return ret;
}

arma::vec _lotriOmega_full_tr28(_lotriOmega_full_omega *fome) {
  arma::vec ret(fome->nTotTheta, arma::fill::none);
  double *ptr = ret.memptr();
  for (int i = 0; i < fome->nomes; ++i) {
    _lotriOmega_ind_omega *ome = &(fome->omes[i]);
    arma::vec tr28 = nlmixr2omega_tr28(ome);
    std::copy(tr28.begin(), tr28.end(), ptr);
    ptr += tr28.size();
  }
  return ret;
}

extern "C" SEXP _lotri_tr28(SEXP inSEXP) {
  _lotriOmega_full_omega p = omegaFromRgetFullOmegaFromSexp(inSEXP);
  arma::vec ret = _lotriOmega_full_tr28(&p);
  SEXP retS = PROTECT(Rf_allocVector(REALSXP, ret.size()));
  double *ptr = REAL(retS);
  std::copy(ret.begin(), ret.end(), ptr);
  UNPROTECT(1);
  return retS;
}
