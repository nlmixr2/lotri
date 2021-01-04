#include "matlist.h"

SEXP nestLotriExpandById(lotriNestGet *ng, SEXP nestLotri, SEXP nestLotriProp, lotriNestInfo *ret) {
  if (ng->extra) {
    // Look for ID
    int found1=0, found2=0;
    SEXP nestN2 = PROTECT(Rf_allocVector(STRSXP, ng->lenNest+ng->extra));
    for (int j = 0; j < ng->lotriLen; ++j) {
      if (found1 == 0 &&
	  !casecmp("id", CHAR(STRING_ELT(ng->names, j)))) {
	found1 = 1;
	SET_STRING_ELT(nestN2, 0, STRING_ELT(ng->names, j));
	SET_VECTOR_ELT(nestLotri, 0, VECTOR_ELT(ng->lotri, j));
      }
      if (found2 == 0 &&
	  !casecmp("id", CHAR(STRING_ELT(ng->lotri0names, j)))) {
	SET_VECTOR_ELT(nestLotriProp, 0, VECTOR_ELT(ng->lotri0, j));
	found2 = 1;
      }
      if (found1 == 1 && found2 == 1) {
	break;
      }
    }
    if (found1 == 0 || found2 == 0) {
      ret->err = 2;
    }
    for (int i = 0; i  < ng->lenNest; ++i) {
      SET_STRING_ELT(nestN2, i+1, STRING_ELT(ng->nestN, i));
    }
    UNPROTECT(1);
    return nestN2;
  }
  return ng->nestN;

}

lotriNestInfo getNestLotri(int lenNest, int extra, int lotriLen,
			   SEXP nestN, SEXP lotri, SEXP names, SEXP lotri0, SEXP lotri0names,
			   SEXP sameC, int *nestI, SEXP nestStart) {
  lotriNestInfo ret;
  lotriNestGet in0;
  in0.lenNest =  lenNest;
  in0.extra = extra;
  in0.lotriLen = lotriLen;
  in0.nestN = nestN;
  in0.lotri = lotri;
  in0.names = names;
  in0.lotri0 = lotri0;
  in0.lotri0names = lotri0names;
  in0.sameC = sameC;
  in0.nestI = nestI;
  in0.nestStart = nestStart;
  ret.err = 0;
  if (TYPEOF(in0.nestStart) != INTSXP || Rf_length(in0.nestStart) != 1) {
    ret.err = 1;
    return ret;
    /* Rf_errorcall(R_NilValue, "'%sStart' needs to be an 'integer' of length 1", what); */
  }
  int pro = 0;
  SEXP nestLotri = PROTECT(Rf_allocVector(VECSXP, in0.lenNest + in0.extra)); pro++;
  SEXP nestLotriProp = PROTECT(Rf_allocVector(VECSXP, in0.lenNest + in0.extra)); pro++;
  SEXP lotriClass = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(lotriClass, 0, Rf_mkChar("lotri"));
  Rf_setAttrib(nestLotri, Rf_install("lotri"), nestLotriProp);
  SEXP nestN2 = PROTECT(nestLotriExpandById(&in0, nestLotri, nestLotriProp, &ret)); pro++;
  Rf_setAttrib(nestLotri, R_NamesSymbol, nestN2);
  Rf_setAttrib(nestLotriProp, R_NamesSymbol, nestN2);
  for (int i = extra; i < lenNest+extra; ++i) {
    int found1 = 0, found2 = 0;
    for (int j = 0; j < lotriLen; ++j) {
      const char *curNest = CHAR(STRING_ELT(nestN2, i));
      if (found1 == 0 &&
	  !strcmp(curNest, CHAR(STRING_ELT(names, j)))) {
	found1 = 1;
	SET_VECTOR_ELT(nestLotri, i, VECTOR_ELT(lotri, j));
      }
      if (found2 == 0 &&
	  !strcmp(curNest, CHAR(STRING_ELT(lotri0names, j)))) {
	// Now assign it
	SET_VECTOR_ELT(nestLotriProp, i, addLotriPropertyAtEnd(lotri0, i, sameC, nestI, extra));
	found2 = 1;
      }
      if (found1 == 1 && found2 == 1) {
	break;
      }
    }
    if (found1 == 0 || found2 == 0) {
      ret.err = 3;
      break;
    }
  }
  SEXP format = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  if (extra) {
    SET_STRING_ELT(format, 0, Rf_mkChar("ETA[%d]"));
  } else {
    SET_STRING_ELT(format, 0, Rf_mkChar("THETA[%d]"));
  }
  Rf_setAttrib(nestLotri, R_ClassSymbol, lotriClass);
  Rf_setAttrib(nestLotri, Rf_install("format"), format);
  Rf_setAttrib(nestLotri, Rf_install("start"), nestStart);
  ret.ret = nestLotri;
  UNPROTECT(pro);
  return ret;
}
