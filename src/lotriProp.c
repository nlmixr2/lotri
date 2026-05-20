#include "matlist.h"

SEXP getLotriProp(SEXP names, R_xlen_t i,
		  SEXP lotriProp,
		  SEXP lotriPropNames, const char *prop) {
  const char *what = CHAR(STRING_ELT(names, i));
  for (R_xlen_t j = Rf_xlength(lotriPropNames); j--;) {
    const char *cur = CHAR(STRING_ELT(lotriPropNames, j));
    if (!strcmp(what, cur)){
      SEXP lotriCur = VECTOR_ELT(lotriProp, j);
      SEXP lotriCurNames = Rf_getAttrib(lotriCur, R_NamesSymbol);
      for (R_xlen_t k = Rf_xlength(lotriCurNames); k--; ) {
	const char *cur2 = CHAR(STRING_ELT(lotriCurNames, k));
	if (!strcmp(cur2, prop)) {
	  return VECTOR_ELT(lotriCur, k);
	}
      }
    }
  }
  return R_NilValue;
}

SEXP blankProp(SEXP names) {
  int pro = 0;
  R_xlen_t len = Rf_xlength(names);
  SEXP lotriProp = PROTECT(Rf_allocVector(VECSXP, len));pro++;
  for (R_xlen_t j = len; j--;) {
    SET_VECTOR_ELT(lotriProp, j, Rf_allocVector(VECSXP, 0));
  }
  Rf_setAttrib(lotriProp, R_NamesSymbol, names);
  UNPROTECT(pro);
  return lotriProp;
}

SEXP _lotriMaxNu(SEXP lotri) {
  SEXP lotriProp = PROTECT(Rf_getAttrib(lotri, Rf_install("lotri")));
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(ret)[0] = 0.0;
  if (Rf_isNull(lotriProp)){
    UNPROTECT(2);
    return ret;
  }
  SEXP lotriPropNames = Rf_getAttrib(lotriProp, R_NamesSymbol);
  SEXP names = Rf_getAttrib(lotri, R_NamesSymbol);
  double maxNu = 0.0;
  for (R_xlen_t i = Rf_xlength(lotri); i--;) {
    SEXP nu = getLotriProp(names, i, lotriProp, lotriPropNames, "nu");
    if (!Rf_isNull(nu) && Rf_length(nu) == 1){
      double tmp=0;
      if (TYPEOF(nu) == REALSXP && maxNu < (tmp = REAL(nu)[0])) {
	maxNu = tmp;
      }
    }
  }
  REAL(ret)[0] = maxNu;
  UNPROTECT(2);
  return ret;
}

SEXP addLotriPropertyAtEnd(SEXP lotri0, R_xlen_t i, SEXP sameC, int *nestI, int extra) {
  // Here we found the lotri property,
  // Create a new list with "same" at the end
  int pro = 0;
  SEXP curProp  = VECTOR_ELT(lotri0, i);
  R_xlen_t curPropN = Rf_xlength(curProp);
  SEXP curPropS = PROTECT(Rf_getAttrib(curProp, R_NamesSymbol)); pro++;
  SEXP newProp  = PROTECT(Rf_allocVector(VECSXP, curPropN+1)); pro++;
  SEXP newPropS = PROTECT(Rf_allocVector(STRSXP, curPropN+1)); pro++;
  for (R_xlen_t k = 0; k < curPropN; ++k) {
    SET_VECTOR_ELT(newProp, k, VECTOR_ELT(curProp, k));
    SET_STRING_ELT(newPropS, k, STRING_ELT(curPropS, k));
  }
  SET_STRING_ELT(newPropS, curPropN, sameC);
  SEXP nestVal = PROTECT(Rf_allocVector(INTSXP, 1)); pro++;
  INTEGER(nestVal)[0] = nestI[i-extra];
  SET_VECTOR_ELT(newProp, curPropN, nestVal);
  Rf_setAttrib(newProp, R_NamesSymbol, newPropS);
  UNPROTECT(pro);
  return newProp;
}

SEXP ampDefault(SEXP cur, SEXP dimn, double val, int pro0, const char * what) {
  if (TYPEOF(cur) != REALSXP) {
    UNPROTECT(pro0);
    Rf_errorcall(R_NilValue, "'%s' needs to be a double", what);
  }
  int pro = 0;
  SEXP names = Rf_getAttrib(cur, R_NamesSymbol);
  R_xlen_t nDim = Rf_xlength(dimn);
  if (Rf_isNull(names)) {
    if (Rf_xlength(cur) == 1){
      SEXP ret = PROTECT(Rf_allocVector(REALSXP, nDim)); pro++;
      double *retD = REAL(ret);
      Rf_setAttrib(ret, R_NamesSymbol, dimn);
      double inVal = REAL(cur)[0];
      for (R_xlen_t i = nDim; i--;) {
	retD[i] = inVal;
      }
      UNPROTECT(pro);
      return ret;
    } else {
      UNPROTECT(pro0);
      Rf_errorcall(R_NilValue, "'%s' needs to be named", what);
    }
  } else {
    R_xlen_t nnames = Rf_xlength(names);
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, nDim)); pro++;
    double *retD = REAL(ret);
    double *in = REAL(cur);
    for (R_xlen_t i = 0; i < nDim; ++i) {
      int found = 0;
      for (R_xlen_t j = 0; j < nnames; ++j) {
	if (!strcmp(CHAR(STRING_ELT(dimn, i)),
		    CHAR(STRING_ELT(names, j)))) {
	  retD[i] = in[j];
	  found = 1;
	  break;
	}
      }
      if (found == 0) {
	retD[i] = val;
      }
    }
    Rf_setAttrib(ret, R_NamesSymbol, dimn);
    UNPROTECT(pro);
    return ret;
  }
  return R_NilValue;
}
