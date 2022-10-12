#include "matlist.h"

SEXP _lotriSep(SEXP lotri, SEXP above, SEXP below,
	       SEXP aboveStart, SEXP belowStart) {
  int pro    = 0;
  SEXP names = PROTECT(Rf_getAttrib(lotri, R_NamesSymbol)); pro++;
  SEXP lotri0 = PROTECT(Rf_getAttrib(lotri, Rf_install("lotri"))); pro++;
  if (Rf_isNull(lotri0)) {
    lotri0 = PROTECT(blankProp(names)); pro++;
  }
  SEXP lotri0names = PROTECT(Rf_getAttrib(lotri0, R_NamesSymbol)); pro++;
  int lotriLen = Rf_length(names);
  if (lotriLen != Rf_length(lotri0)) {
    UNPROTECT(pro);
    Rf_errorcall(R_NilValue, "'lotri' malformed");
  }
  SEXP aboveN;
  SEXP belowN = PROTECT(Rf_getAttrib(below, R_NamesSymbol)); pro++;
  if (Rf_isNull(belowN)){
    UNPROTECT(pro);
    Rf_errorcall(R_NilValue, "'below' needs to be named");
  }
  if (TYPEOF(below) != INTSXP) {
    UNPROTECT(pro);
    Rf_errorcall(R_NilValue, "'below' needs to be an integer");
  }
  int *belowI = INTEGER(below);
  int *aboveI;
  int lenAbove = Rf_length(above);
  int lenBelow = Rf_length(below);
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 2)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 2)); pro++;
  SET_STRING_ELT(retN, 0, Rf_mkChar("above"));
  SET_STRING_ELT(retN, 1, Rf_mkChar("below"));
  SEXP sameC = PROTECT(Rf_mkChar("same")); pro++;
  Rf_setAttrib(ret, R_NamesSymbol, retN);
  if (lenAbove == 0) {
    SET_VECTOR_ELT(ret, 0, R_NilValue);
  } else {
    aboveN = PROTECT(Rf_getAttrib(above, R_NamesSymbol)); pro++;
    if (Rf_isNull(aboveN)){
      Rf_errorcall(R_NilValue, "'above' needs to be named");
    }
    if (TYPEOF(above) != INTSXP) {
      Rf_errorcall(R_NilValue, "'above' needs to be an integer");
    }
    aboveI = INTEGER(above);
    lotriNestInfo curLT =getNestLotri(lenAbove, 0, lotriLen, aboveN, lotri, names, lotri0, lotri0names,
				     sameC, aboveI, aboveStart);
    if (curLT.err == 1) {
      UNPROTECT(pro);
      Rf_errorcall(R_NilValue, "'aboveStart' needs to be an 'integer' of length 1");
    } else if (curLT.err == 2) {
      UNPROTECT(pro);
      Rf_errorcall(R_NilValue, "'id' not found in 'lotri' matrix");
    } else if (curLT.err == 3) {
      UNPROTECT(pro);
      Rf_errorcall(R_NilValue, "'above' names do not match 'lotri' matrix");
    }
    SET_VECTOR_ELT(ret, 0, PROTECT(curLT.ret)); pro++;
  }
  lotriNestInfo curLT2 =  getNestLotri(lenBelow, 1, lotriLen,
				       belowN, lotri, names, lotri0, lotri0names,
				       sameC, belowI, belowStart);
  if (curLT2.err == 1) {
    UNPROTECT(pro);
    Rf_errorcall(R_NilValue, "'belowStart' needs to be an 'integer' of length 1");
  } else if (curLT2.err == 2) {
    UNPROTECT(pro);
    Rf_errorcall(R_NilValue, "'id' not found in 'lotri' matrix");
  } else if (curLT2.err == 3) {
    UNPROTECT(pro);
    Rf_errorcall(R_NilValue, "'below' names do not match 'lotri' matrix");
  }
  SET_VECTOR_ELT(ret, 1, PROTECT(curLT2.ret)); pro++;

  UNPROTECT(pro);
  return ret;
}

SEXP _lotriAllNames(SEXP lotri) {
  int pro = 0;
  if (Rf_isMatrix(lotri)){
    SEXP dimn = PROTECT(Rf_getAttrib(lotri, R_DimNamesSymbol)); pro++;
    if (dimn == R_NilValue) {
      SEXP retN = PROTECT(Rf_allocVector(STRSXP, 0)); pro++;
      UNPROTECT(pro);
      return retN;
    }
    SEXP colnames = PROTECT(VECTOR_ELT(dimn, 1)); pro++;
    if (Rf_isNull(colnames)){
      colnames = PROTECT(VECTOR_ELT(dimn, 0)); pro++;
      if (Rf_isNull(colnames)) {
	SEXP retN = PROTECT(Rf_allocVector(STRSXP, 0)); pro++;
	UNPROTECT(pro);
	return retN;
      }
    }
    UNPROTECT(pro);
    return colnames;
  } else {
    int type = TYPEOF(lotri);
    if (type == VECSXP) {
      int intN=0;
      for (int i = Rf_length(lotri); i--;){
	intN +=
	  Rf_length(VECTOR_ELT(Rf_getAttrib(VECTOR_ELT(lotri, i),
					    R_DimNamesSymbol), 1));
      }
      int j = 0;
      SEXP ret = PROTECT(Rf_allocVector(STRSXP, intN)); pro++;
      for (int i = Rf_length(lotri); i--;){
	SEXP cur = VECTOR_ELT(Rf_getAttrib(VECTOR_ELT(lotri, i),
					   R_DimNamesSymbol), 1);
	for (int k = 0; k < Rf_length(cur); ++k) {
	  /* SET_STRING_ELT(retN, curBand+j, Rf_mkChar(out)); */
	  SET_STRING_ELT(ret, j++, STRING_ELT(cur, k));
	}
      }
      UNPROTECT(pro);
      return ret;
    } else {
      UNPROTECT(pro);
      Rf_errorcall(R_NilValue, _("not a matrix or lotri matrix"));
    }
  }
}



SEXP _isLotri(SEXP lotri) {
  SEXP ret = PROTECT(Rf_allocVector(LGLSXP, 1));
  if (Rf_isNull(lotri)) {
    INTEGER(ret)[0] = 0;
    UNPROTECT(1);
    return ret;
  }
  SEXP lotriProp = Rf_getAttrib(lotri, Rf_install("lotri"));
  if (Rf_isNull(lotriProp)) {
    if (TYPEOF(lotri) == VECSXP){
      int isL = 1;
      for (int i = Rf_length(lotri); i--;){
	SEXP cur = VECTOR_ELT(lotri, i);
	if (Rf_isMatrix(cur)) {
	  SEXP dimnames = Rf_getAttrib(cur, R_DimNamesSymbol);
	  if (Rf_isNull(dimnames)) {
	    isL = 0;
	    break;
	  }
	} else {
	  isL = 0;
	  break;
	}
      }
      INTEGER(ret)[0] = isL;
    } else {
      INTEGER(ret)[0] = 0;
    }
  } else {
    INTEGER(ret)[0] = 1;
  }
  UNPROTECT(1);
  return ret;
}

void R_init_lotri(DllInfo *info){
  R_CallMethodDef callMethods[]  = {
    {"_lotriLstToMat", (DL_FUNC) &_lotriLstToMat, 4},
    {"_asLotriMat", (DL_FUNC) &_asLotriMat, 3},
    {"_lotriSep", (DL_FUNC) &_lotriSep, 5},
    {"_lotriAllNames", (DL_FUNC) &_lotriAllNames, 1},
    {"_lotriGetBounds", (DL_FUNC) &_lotriGetBounds, 3},
    {"_lotriMaxNu", (DL_FUNC) &_lotriMaxNu, 1},
    {"_isLotri", (DL_FUNC) &_isLotri, 1},
    {NULL, NULL, 0}
  };
  R_RegisterCCallable("lotri", "_lotriLstToMat", (DL_FUNC) _lotriLstToMat);
  R_RegisterCCallable("lotri", "_asLotriMat", (DL_FUNC) _asLotriMat);
  R_RegisterCCallable("lotri", "_lotriSep", (DL_FUNC) _lotriSep);
  R_RegisterCCallable("lotri", "_lotriAllNames", (DL_FUNC) _lotriAllNames);
  R_RegisterCCallable("lotri", "_lotriGetBounds", (DL_FUNC) _lotriGetBounds);
  R_RegisterCCallable("lotri", "_lotriMaxNu", (DL_FUNC) _lotriMaxNu);
  R_RegisterCCallable("lotri", "_isLotri", (DL_FUNC) _isLotri);
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
