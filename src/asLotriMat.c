#include "matlist.h"

SEXP _asLotriMatGen(SEXP x, SEXP extra, SEXP def, SEXP dims, SEXP dimn, const char *defVal) {
  int pro = 0;
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 1)); pro++;
  SET_VECTOR_ELT(ret, 0, x);
  Rf_setAttrib(ret, R_NamesSymbol, def);
  SEXP lotriClass = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(lotriClass, 0, Rf_mkChar("lotri"));

  // Now setup extra
  int nNull = 0;
  int lExtra = Rf_length(extra);
  if (lExtra == 0) {
    // Here extra isn't added.
    Rf_setAttrib(ret, R_ClassSymbol, lotriClass);
    UNPROTECT(pro);
    return ret;
  } else if (!strcmp(defVal, "")) {
    UNPROTECT(pro);
    Rf_errorcall(R_NilValue, "extra properties need default try 'lotri(matrix,x=3,default=\"id\")'");
  }
  SEXP extraNames = Rf_getAttrib(extra, R_NamesSymbol);
  for (int i = lExtra; i--;) {
    if (Rf_isNull(VECTOR_ELT(extra, i))) {
      nNull++;
    }
  }
  SEXP lotri = PROTECT(Rf_allocVector(VECSXP, 1)); pro++;
  SEXP fextra = PROTECT(Rf_allocVector(VECSXP, lExtra-nNull)); pro++;
  Rf_setAttrib(lotri, R_NamesSymbol, def);
  SEXP fextraN = PROTECT(Rf_allocVector(STRSXP, lExtra-nNull)); pro++;
  int j = 0;
  for (int i = lExtra; i--;) {
    if (!Rf_isNull(VECTOR_ELT(extra, i))){
      SEXP curNameS = STRING_ELT(extraNames, i);
      const char *curName = CHAR(curNameS);
      if (!strcmp("lower", curName)) {
	SET_VECTOR_ELT(fextra, j, ampDefault(VECTOR_ELT(extra, i), dimn, R_NegInf, pro, "lower"));
      } else if (!strcmp("upper", curName)) {
	SET_VECTOR_ELT(fextra, j, ampDefault(VECTOR_ELT(extra, i), dimn, R_PosInf, pro, "upper"));
      } else {
	SET_VECTOR_ELT(fextra, j, VECTOR_ELT(extra, i));
      }
      SET_STRING_ELT(fextraN, j, curNameS);
      j++;
    }
  }
  SET_VECTOR_ELT(lotri, 0, fextra);
  Rf_setAttrib(fextra, R_NamesSymbol, fextraN);
  Rf_setAttrib(ret, Rf_install("lotri"), lotri);
  Rf_setAttrib(ret, R_ClassSymbol, lotriClass);
  UNPROTECT(pro);
  return ret;
}

// put into C to allow calling from RxODE from C.
SEXP _asLotriMat(SEXP x, SEXP extra, SEXP def) {
  if (TYPEOF(def) != STRSXP || Rf_length(def) != 1) {
    Rf_errorcall(R_NilValue, _("'default' must be a 'string' of length 1"));
  }
  if (!Rf_isMatrix(x)) {
    Rf_errorcall(R_NilValue, _("'x' needs to be a completely named matrix"));
  }
  SEXP dims = Rf_getAttrib(x, R_DimNamesSymbol);
  if (Rf_isNull(dims)){
    Rf_errorcall(R_NilValue, _("'x' needs to be a completely named matrix"));
  }
  SEXP dimn = VECTOR_ELT(dims, 0);
  if (Rf_isNull(dimn)) {
    Rf_errorcall(R_NilValue, _("'x' needs to be a completely named matrix"));
  }
  if (Rf_isNull(VECTOR_ELT(dims, 1))) {
    Rf_errorcall(R_NilValue, _("'x' needs to be a completely named matrix"));
  }
  const char *defVal = CHAR(STRING_ELT(def, 0));
  if (TYPEOF(extra) != VECSXP) {
    Rf_errorcall(R_NilValue, _("'extra' must be a list"));
  }
  return _asLotriMatGen(x, extra, def, dims, dimn, defVal);
}
