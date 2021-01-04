#include "matlist.h"

SEXP lotriToLstMat(SEXP lotri) {
  SEXP lotriProp = Rf_getAttrib(lotri, Rf_install("lotri"));
  if (Rf_isNull(lotriProp)) {
    return lotri;
  }
  SEXP lotriNames = Rf_getAttrib(lotri, R_NamesSymbol);
  SEXP lotriPropNames = Rf_getAttrib(lotriProp, R_NamesSymbol);
  int pro=0;
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, Rf_length(lotri))); pro++;
  int nsame;
  for (int i = Rf_length(ret); i--;) {
    nsame = getSame(lotriNames, i, lotriProp, lotriPropNames);
    if (nsame > 1){
      SEXP cur = PROTECT(Rf_allocVector(VECSXP, 2)); pro++;
      SET_VECTOR_ELT(cur, 0, VECTOR_ELT(lotri, i));
      SEXP ns = PROTECT(Rf_allocVector(INTSXP, 1)); pro++;
      INTEGER(ns)[0] = nsame;
      SET_VECTOR_ELT(cur, 1, ns);
      SET_VECTOR_ELT(ret, i, cur);
    } else {
      SET_VECTOR_ELT(ret, i, VECTOR_ELT(lotri, i));
    }
  }
  UNPROTECT(pro);
  return ret;
}

lotriInfo assertCorrectMatrixProperties(SEXP lst_, SEXP format, SEXP startNum, int *named) {
  int type = TYPEOF(lst_);
  if (type != VECSXP) {
    if (isSymNameMat(lst_, *named)) {
      lotriInfo li;
      li.sym = 1;
      li.lst = R_NilValue;
      return li;
    }
    Rf_errorcall(R_NilValue, _("expects a list named symmetric matrices"));
  }
  lotriInfo li = _lotriLstToMat0(lst_, format, startNum);
  PROTECT(li.lst); 
  if (li.err == 1) {
    UNPROTECT(1);
    Rf_errorcall(R_NilValue, _("'format' must be a single length string or NULL"));
  }
  if (li.err == 2) {
    UNPROTECT(1);
    Rf_errorcall(R_NilValue, _("when format is specified, 'startNum' must be a single integer"));
  }
  UNPROTECT(1);
  return li;
}

static inline void lotriLstToMatFillInMatrix(double *retd, int nsame, int type, int named, int totN, int totdim,
					     SEXP retN, SEXP colnames, int *curBand, lotriInfo *li,
					     SEXP cur) {
  for (int cursame = nsame; cursame--;){
    if (type == REALSXP) {
      double *curd = REAL(cur);
      for (int j = 0; j  < totN; ++j) {
	memcpy(&retd[totdim*(*curBand+j)+(*curBand)],
	       &curd[totN*j], sizeof(double)*totN);
	// Repeats dim names of repeated matrices
	if (named) {
	  setStrElt(retN, colnames, (*curBand), j,
		    li->fmt, li->doFormat, &(li->counter), nsame);
	}
      }
    } else {
      int *curi = INTEGER(cur);
      for (int j = 0; j < totN; ++j) {
	double *to = &retd[totdim*(*curBand+j)+(*curBand)];
	double *last = to + totN; // N - count
	int *from = &curi[totN*j];
	while (to != last) {
	  *(to++) = (double)(*(from++));
	}
	if (named) {
	  setStrElt(retN, colnames, (*curBand), j,
		    li->fmt, li->doFormat, &(li->counter), nsame);
	}
      }
    }
    *curBand += totN;
  }
}

SEXP _lotriLstToMat(SEXP lst_, SEXP format, SEXP startNum) {
  int type, totN, pro = 0;
  int named = 2;
  lotriInfo li = assertCorrectMatrixProperties(lst_, format, startNum, &named);
  if (li.sym) return lst_;
  PROTECT(li.lst); pro++;
  int len = Rf_length(li.lst);
  int totdim = 0;
  int i;
  if (len == 2) {
    int repN = isSingleInt(VECTOR_ELT(li.lst, 1), NA_INTEGER);
    if (repN == NA_INTEGER){
    } else if (repN > 0) {
      if (isSymNameMat(VECTOR_ELT(li.lst, 0), named)){
	SEXP new = PROTECT(Rf_allocVector(VECSXP, 1)); pro++;
	SET_VECTOR_ELT(new, 0, li.lst);
	SEXP ret = _lotriLstToMat(new, format, startNum);
	UNPROTECT(pro);
	return ret;
      }
    }
  }
  for (i = 0; i < len; ++i) {
    totdim += getCheckDim(li.lst, i, &named);
  }
  SEXP ret = PROTECT(Rf_allocMatrix(REALSXP, totdim, totdim)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, totdim)); pro++;
  double *retd = REAL(ret);
  // Initialize to zero
  memset(retd, 0, sizeof(double)*totdim*totdim);
  // Now use memcpy/ integer conversion to c
  SEXP cur;
  int curBand = 0;
  SEXP dimnames, colnames, sameS;
  int nsame;
  for (i = 0; i < len; ++i) {
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
    if (named) {
      dimnames = Rf_getAttrib(cur, R_DimNamesSymbol);
      colnames = VECTOR_ELT(dimnames, 1);
    }
    lotriLstToMatFillInMatrix(retd, nsame, type, named, totN, totdim,
			      retN, colnames, &curBand, &li, cur);
  }
  if (named) {
    dimnames = PROTECT(Rf_allocVector(VECSXP, 2)); pro++;
    SET_VECTOR_ELT(dimnames, 0, retN);
    SET_VECTOR_ELT(dimnames, 1, retN);
    Rf_setAttrib(ret, R_DimNamesSymbol, dimnames);
  }
  UNPROTECT(pro);
  return ret;
}
