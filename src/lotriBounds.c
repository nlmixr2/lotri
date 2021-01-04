#include "matlist.h"

SEXP _lotriAssumeUnbounded(SEXP lst_) {
  int pro=0;
  SEXP names = PROTECT(_lotriAllNames(lst_)); pro++;
  int len = Rf_length(names);
  SEXP boundLower = PROTECT(Rf_allocVector(REALSXP, len)); pro++;
  SEXP boundUpper = PROTECT(Rf_allocVector(REALSXP, len)); pro++;
  Rf_setAttrib(boundLower, R_NamesSymbol, names);
  Rf_setAttrib(boundUpper, R_NamesSymbol, names);
  double *bL = REAL(boundLower);
  double *bU = REAL(boundUpper);
  for (int j = len; j--; ){
    bL[j] = R_NegInf;
    bU[j] = R_PosInf;
  }
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 2)); pro++;
  SET_VECTOR_ELT(ret, 0, boundLower);
  SET_VECTOR_ELT(ret, 1, boundUpper);
  SEXP retFN = PROTECT(Rf_allocVector(STRSXP, 2)); pro++;
  SET_STRING_ELT(retFN, 0, Rf_mkChar("lower"));
  SET_STRING_ELT(retFN, 1, Rf_mkChar("upper"));
  Rf_setAttrib(ret, R_NamesSymbol, retFN);
  UNPROTECT(pro);
  return ret;
}

SEXP _lotriGetBounds(SEXP lst_, SEXP format, SEXP startNum) {
  int type = TYPEOF(lst_), totN;
  if (type != VECSXP) {
    Rf_errorcall(R_NilValue, _("expects lotri matrix"));
  }
  if (Rf_isNull(Rf_getAttrib(lst_, Rf_install("lotri")))) {
    return _lotriAssumeUnbounded(lst_);
  }
  int pro = 0;
  SEXP lotriProp = PROTECT(Rf_getAttrib(lst_, Rf_install("lotri"))); pro++;
  SEXP lotriPropNames = PROTECT(Rf_getAttrib(lotriProp, R_NamesSymbol)); pro++;
  SEXP names = PROTECT(Rf_getAttrib(lst_, R_NamesSymbol)); pro++;
  lotriInfo li = _lotriLstToMat0(lst_, format, startNum);
  PROTECT(li.lst); pro++;
  if (li.err == 1) {
    UNPROTECT(pro);
    Rf_errorcall(R_NilValue, _("'format' must be a single length string or NULL"));
  }
  if (li.err == 2) {
    UNPROTECT(pro);
    Rf_errorcall(R_NilValue, _("when format is specified, 'startNum' must be a single integer"));
  }
  int len = Rf_length(li.lst);
  int totdim=0;
  for (int i = 0; i < len; ++i) {
    totdim += getCheckDim(li.lst, i, 1);
  }
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, totdim)); pro++;
  SEXP boundLower = PROTECT(Rf_allocVector(REALSXP, totdim)); pro++;
  double *boundLowerD = REAL(boundLower);
  SEXP boundUpper = PROTECT(Rf_allocVector(REALSXP, totdim)); pro++;
  double *boundUpperD = REAL(boundUpper);
  int nsame, j;
  SEXP cur, sameS, dimnames, colnames;
  int curBand = 0;
  int badUpper = 0, badLower = 0;
  for (int i = 0; i < len; ++i) {
    cur = VECTOR_ELT(li.lst, i);
    type = TYPEOF(cur);
    nsame = 1;
    if (type == VECSXP) {
      sameS = VECTOR_ELT(cur, 1);
      nsame = isSingleInt(sameS, 1);
      cur = VECTOR_ELT(cur, 0);
      type = TYPEOF(cur);
    }
    totN = Rf_ncols(cur);
    dimnames = Rf_getAttrib(cur, R_DimNamesSymbol);
    colnames = VECTOR_ELT(dimnames, 1);
    SEXP upper = getLotriProp(names, i, lotriProp, lotriPropNames, "upper");
    if (setUpperLower(upper, colnames, boundUpperD, curBand, R_PosInf,
		      "upper",nsame)) {
      badUpper=1;
      break;
    }
    SEXP lower = getLotriProp(names, i, lotriProp, lotriPropNames, "lower");
    if (setUpperLower(lower, colnames, boundLowerD, curBand, R_NegInf,
		  "lower", nsame)) {
      badLower=1;
      break;
    }
    for (int cursame = nsame; cursame--;){
      // Repeats dim names of repeated matrices
      for (j = 0; j  < totN; ++j) {
	setStrElt(retN, colnames, curBand, j,
		  li.fmt, li.doFormat, &li.counter, nsame);
      }
      curBand += totN;
    }
  }
  if (badUpper) {
    Rf_errorcall(R_NilValue, _("cannot figure out valid 'upper' properties"));
    UNPROTECT(pro);
  }
  if (badLower) {
    Rf_errorcall(R_NilValue, _("cannot figure out valid 'lower' properties"));
    UNPROTECT(pro);
  }
  Rf_setAttrib(boundLower, R_NamesSymbol, retN);
  Rf_setAttrib(boundUpper, R_NamesSymbol, retN);
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 2)); pro++;
  SET_VECTOR_ELT(ret, 0, boundLower);
  SET_VECTOR_ELT(ret, 1, boundUpper);
  SEXP retFN = PROTECT(Rf_allocVector(STRSXP, 2)); pro++;
  SET_STRING_ELT(retFN, 0, Rf_mkChar("lower"));
  SET_STRING_ELT(retFN, 1, Rf_mkChar("upper"));
  Rf_setAttrib(ret, R_NamesSymbol, retFN);
  UNPROTECT(pro);
  return ret;
}
