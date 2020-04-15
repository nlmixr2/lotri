#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("RxODE", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

int getCheckDim(SEXP lst, int i) {
  SEXP cur = VECTOR_ELT(lst, i);
  int type = TYPEOF(cur);
  if (type == INTSXP || type == REALSXP) {
    if (Rf_isMatrix(cur)){
      int nrows = Rf_nrows(cur);
      int ncols = Rf_ncols(cur);
      if (nrows == ncols) {
	SEXP dimn = Rf_getAttrib(cur, R_DimNamesSymbol);
	if (dimn != R_NilValue) {
	  return nrows;
	}
      }
    }
  }
  Rf_error(_("list element %d is not a symmetric named matrix"), i+1);
  return 0;
}

SEXP _lotriLstToMat(SEXP lst) {
  if (TYPEOF(lst) != VECSXP) {
    Rf_error(_("expects a list named symmetric matrices"));
  }
  int len = Rf_length(lst);
  int pro = 0;
  int totdim = 0;
  int i, j;
  for (i = 0; i < len; ++i) {
    totdim += getCheckDim(lst, i);
  }
  SEXP ret = PROTECT(Rf_allocMatrix(REALSXP, totdim, totdim)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, totdim)); pro++;
  double *retd = REAL(ret);
  // Initialize to zero
  memset(retd, 0, sizeof(double)*totdim*totdim);
  // Now use memcpy/ integer conversion to c
  SEXP cur;
  int type, totN;
  double *curd;
  int *curi;
  int curBand = 0;
  SEXP dimnames, colnames;
  for (i = 0; i < len; ++i) {
    cur = VECTOR_ELT(lst, i);
    type = TYPEOF(cur);
    totN = Rf_ncols(cur);
    dimnames = Rf_getAttrib(cur, R_DimNamesSymbol);
    colnames = VECTOR_ELT(dimnames, 1);
    if (type == REALSXP) {
      curd = REAL(cur);
      for (j = 0; j  < totN; ++j) {
	memcpy(&retd[totdim*(curBand+j)+curBand],
	       &curd[totN*j], sizeof(double)*totN);
	SET_STRING_ELT(retN, curBand+j, STRING_ELT(colnames, j));
      }
    } else {
      curi = INTEGER(cur);
      for (j = 0; j < totN; ++j) {
	double *to = &retd[totdim*(curBand+j)+curBand];
	double *last = to + totN; // N - count
	int *from = &curi[totN*j];
	while (to != last) {
	  *(to++) = (double)(*(from++));
	}
	SET_STRING_ELT(retN, curBand+j, STRING_ELT(colnames, j));
      }
    }
    curBand += totN;
  }
  dimnames = PROTECT(Rf_allocVector(VECSXP, 2)); pro++;
  SET_VECTOR_ELT(dimnames, 0, retN);
  SET_VECTOR_ELT(dimnames, 1, retN);
  Rf_setAttrib(ret, R_DimNamesSymbol, dimnames);
  UNPROTECT(pro);
  return ret;
}

void R_init_lotri(DllInfo *info){
  R_CallMethodDef callMethods[]  = {
    {"_lotriLstToMat", (DL_FUNC) &_lotriLstToMat, 1},
    {NULL, NULL, 0}
  };
  R_RegisterCCallable("lotri", "_lotriLstToMat", (DL_FUNC) _lotriLstToMat);
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
