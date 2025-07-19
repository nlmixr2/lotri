#include "matlist.h"
#include "rcm.h"
#include "nearPD.h"
#include "omegaR.h"

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
  SEXP lotriProp = Rf_getAttrib(lotri, Rf_install("lotri"));
  SEXP ret = PROTECT(Rf_allocVector(LGLSXP, 1));
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


void lotriFunNoFree(void *ptr) {
  return;
}

SEXP _lotriNearPD_(SEXP, SEXP, SEXP,  SEXP, SEXP,
                   SEXP, SEXP, SEXP, SEXP, SEXP);


/**
 * getLotriPointers - Creates and returns a list of external pointers to various C functions.
 *
 * This function creates external pointers for several C functions and registers finalizers
 * for them to ensure proper memory management. It then returns these pointers in an R list.
 *
 * @return A list of external pointers to C functions.
 */
SEXP _getLotriPointers(void) {
  int pro = 0;  // Counter for the number of PROTECT calls

  // Create an external pointer for _lotriLstToMat
  SEXP lotriLstToMatPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_lotriLstToMat, R_NilValue, R_NilValue)); pro++;

  // Create an external pointer for _asLotriMat
  SEXP asLotriMatPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_asLotriMat, R_NilValue, R_NilValue)); pro++;

  // Create an external pointer for _lotriSep
  SEXP lotriSepPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_lotriSep,
                                                 R_NilValue, R_NilValue)); pro++;

  // Create an external pointer for _lotriAllNames
  SEXP lotriAllNamesPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_lotriAllNames,
                                                      R_NilValue, R_NilValue)); pro++;

  // Create an external pointer for _lotriGetBounds
  SEXP lotriGetBoundsPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_lotriGetBounds,
                                                       R_NilValue, R_NilValue)); pro++;

  // Create an external pointer for _lotriMaxNu
  SEXP lotriMaxNuPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_lotriMaxNu,
                                                   R_NilValue, R_NilValue)); pro++;

  // Create an external pointer for _isLotri
  SEXP isLotriPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_isLotri,
                                                R_NilValue, R_NilValue)); pro++;

  SEXP lotriRcm = PROTECT(R_MakeExternalPtrFn((DL_FUNC) &_lotri_rcm_,
                                              R_NilValue, R_NilValue)); pro++;

  SEXP lotriNearPD = PROTECT(R_MakeExternalPtrFn((DL_FUNC) &lotriNearPDc,
                                                 R_NilValue, R_NilValue)); pro++;

  SEXP lotriNearPDsexp = PROTECT(R_MakeExternalPtrFn((DL_FUNC) &_lotriNearPD_,
                                                     R_NilValue, R_NilValue)); pro++;


  // Create an R list to hold the external pointers
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 10)); pro++;
  SET_VECTOR_ELT(ret, 0, lotriLstToMatPtr);
  SET_VECTOR_ELT(ret, 1, asLotriMatPtr);
  SET_VECTOR_ELT(ret, 2, lotriSepPtr);
  SET_VECTOR_ELT(ret, 3, lotriAllNamesPtr);
  SET_VECTOR_ELT(ret, 4, lotriGetBoundsPtr);
  SET_VECTOR_ELT(ret, 5, lotriMaxNuPtr);
  SET_VECTOR_ELT(ret, 6, isLotriPtr);
  SET_VECTOR_ELT(ret, 7, lotriRcm);
  SET_VECTOR_ELT(ret, 8, lotriNearPD);
  SET_VECTOR_ELT(ret, 9, lotriNearPDsexp);

  // Create an R character vector to hold the names of the list elements
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 10)); pro++;
  SET_STRING_ELT(retN, 0, Rf_mkChar("lotriLstToMat"));
  SET_STRING_ELT(retN, 1, Rf_mkChar("asLotriMat"));
  SET_STRING_ELT(retN, 2, Rf_mkChar("lotriSep"));
  SET_STRING_ELT(retN, 3, Rf_mkChar("lotriAllNames"));
  SET_STRING_ELT(retN, 4, Rf_mkChar("lotriGetBounds"));
  SET_STRING_ELT(retN, 5, Rf_mkChar("lotriMaxNu"));
  SET_STRING_ELT(retN, 6, Rf_mkChar("isLotri"));
  SET_STRING_ELT(retN, 7, Rf_mkChar("lotriRcm"));
  SET_STRING_ELT(retN, 8, Rf_mkChar("lotriNearPDc"));
  SET_STRING_ELT(retN, 9, Rf_mkChar("lotriNearPDsexp"));

  // Set the names attribute of the list
  Rf_setAttrib(ret, R_NamesSymbol, retN);

  // Unprotect all protected objects
  UNPROTECT(pro);

  // Return the list of external pointers
  return ret;
}


void R_init_lotri(DllInfo *info){
  R_CallMethodDef callMethods[]  = {
    {"_lotriOmega_getBuiltinSize", (DL_FUNC) &_lotriOmega_getBuiltinSize, 0},
    {"_lotriNearPD_",(DL_FUNC) &_lotriNearPD_, 10},
    {"_lotri_rcm_", (DL_FUNC) &_lotri_rcm_, 1},
    {"_getLotriPointers", (DL_FUNC) &_getLotriPointers, 0},
    {"_lotriLstToMat", (DL_FUNC) &_lotriLstToMat, 4},
    {"_asLotriMat", (DL_FUNC) &_asLotriMat, 3},
    {"_lotriSep", (DL_FUNC) &_lotriSep, 5},
    {"_lotriAllNames", (DL_FUNC) &_lotriAllNames, 1},
    {"_lotriGetBounds", (DL_FUNC) &_lotriGetBounds, 3},
    {"_lotriMaxNu", (DL_FUNC) &_lotriMaxNu, 1},
    {"_isLotri", (DL_FUNC) &_isLotri, 1},
    {NULL, NULL, 0}
  };
  // After rxode2 is accepted without the binary linkages, these will be removed
  R_RegisterCCallable("lotri", "_lotriLstToMat", (DL_FUNC) _lotriLstToMat);
  R_RegisterCCallable("lotri", "_asLotriMat", (DL_FUNC) _asLotriMat);
  R_RegisterCCallable("lotri", "_lotriSep", (DL_FUNC) _lotriSep);
  R_RegisterCCallable("lotri", "_lotriAllNames", (DL_FUNC) _lotriAllNames);
  R_RegisterCCallable("lotri", "_lotriGetBounds", (DL_FUNC) _lotriGetBounds);
  R_RegisterCCallable("lotri", "_lotriMaxNu", (DL_FUNC) _lotriMaxNu);
  // End proposed of removal
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
