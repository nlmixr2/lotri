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

static inline int isSymNameMat(SEXP cur) {
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
  return 0;
}

static inline int isSingleInt(SEXP in) {
  int type = TYPEOF(in);
  if (type == INTSXP && Rf_length(in) == 1) {
    if (!Rf_isMatrix(in)) return INTEGER(in)[0];
  } else if (type == REALSXP && Rf_length(in) == 1) {
    if (!Rf_isMatrix(in)) return (int)(REAL(in)[0]);
  }
  return NA_INTEGER;
}

int getCheckDim(SEXP lst, int i) {
  SEXP cur = VECTOR_ELT(lst, i);
  int type = TYPEOF(cur);
  int same=1;
  if (type == VECSXP) {
    if (Rf_length(cur) != 2){
      Rf_error(_("when repeating matrices you need to use 'list(mat, n)'"));
    }
    same = isSingleInt(VECTOR_ELT(cur, 1));
    if (same == NA_INTEGER) {
      Rf_error(_("you can only repeat a matrix a single positive number of times"));
    }
    if (same <= 0) {
      Rf_error(_("you need to repeat a matrix a positive number of times"));
    }
    cur = VECTOR_ELT(cur, 0);
    type = TYPEOF(cur);
  }
  int ret = isSymNameMat(cur);
  if (ret){
    return ret*same;
  } else {
    Rf_error(_("list element %d is not a symmetric named matrix"), i+1);
  }
  return 0;
}

static inline int setStrElt(SEXP retN, SEXP colnames, int curBand, int j,
			    const char *fmt, int doFormat, int *cnt, int nsame) {
  if (doFormat && nsame > 1) {
    char out[100];
    int cx = snprintf( out, 100, fmt, cnt[0]++);
    SET_STRING_ELT(retN, curBand+j, Rf_mkChar(out));
    return cx;
  } else {
    SET_STRING_ELT(retN, curBand+j, STRING_ELT(colnames, j));
  }
  return 0;
}

SEXP _lotriLstToMat(SEXP lst, SEXP format, SEXP startNum) {
  int type = TYPEOF(lst), totN;
  if (type != VECSXP) {
    if (isSymNameMat(lst)) {
      return lst;
    }
    Rf_error(_("expects a list named symmetric matrices"));
  }
  int fmtType = TYPEOF(format);
  const char *fmt;
  int doFormat = 0;
  if (fmtType == STRSXP && Rf_length(format) == 1) {
    fmt = CHAR(STRING_ELT(format, 0));
    doFormat=1;
  } else if (fmtType) {
    Rf_error(_("'format' must be a single length string or NULL"),
	     fmtType);
  }
  int counter = 0;
  if (doFormat) {
    counter = isSingleInt(startNum);
    if (counter == NA_INTEGER){
      Rf_error(_("When format is specified, 'startNum' must be a single integer"));
    }
  }
  int len = Rf_length(lst);
  int pro = 0;
  int totdim = 0;
  int i, j;
  if (len == 2) {
    int repN = isSingleInt(VECTOR_ELT(lst, 1));
    if (repN == NA_INTEGER){
    } else if (repN > 0) {
      if (isSymNameMat(VECTOR_ELT(lst, 0))){
	SEXP new = PROTECT(Rf_allocVector(VECSXP, 1)); pro++;
	SET_VECTOR_ELT(new, 0, lst);
	SEXP ret = _lotriLstToMat(new, format, startNum);
	UNPROTECT(pro);
	return ret;
      }
    }
  }
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
  double *curd;
  int *curi;
  int curBand = 0;
  SEXP dimnames, colnames, sameS;
  int nsame;
  for (i = 0; i < len; ++i) {
    cur = VECTOR_ELT(lst, i);
    type = TYPEOF(cur);
    nsame = 1;
    if (type == VECSXP) {
      sameS = VECTOR_ELT(cur, 1);
      type = TYPEOF(sameS);
      if (type == INTSXP) {
	nsame = INTEGER(sameS)[0];
      } else {
	nsame = (int)REAL(sameS)[0];
      }
      cur = VECTOR_ELT(cur, 0);
      type = TYPEOF(cur);
    }
    totN = Rf_ncols(cur);
    dimnames = Rf_getAttrib(cur, R_DimNamesSymbol);
    colnames = VECTOR_ELT(dimnames, 1);
    for (int cursame = nsame; cursame--;){
      if (type == REALSXP) {
	curd = REAL(cur);
	for (j = 0; j  < totN; ++j) {
	  memcpy(&retd[totdim*(curBand+j)+curBand],
		 &curd[totN*j], sizeof(double)*totN);
	  // Repeats dim names of repeated matrices
	  setStrElt(retN, colnames, curBand, j,
		    fmt, doFormat, &counter, nsame);
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
	  setStrElt(retN, colnames, curBand, j,
		    fmt, doFormat, &counter, nsame);
	}
      }
      curBand += totN;
    }
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
    {"_lotriLstToMat", (DL_FUNC) &_lotriLstToMat, 3},
    {NULL, NULL, 0}
  };
  R_RegisterCCallable("lotri", "_lotriLstToMat", (DL_FUNC) _lotriLstToMat);
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
