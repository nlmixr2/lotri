#ifndef __lotriLstToMat_H__
#define __lotriLstToMat_H__
#include "matlist.h"

typedef struct lotriInfo {
  SEXP lst;
  int doFormat;
  const char *fmt;
  int counter;
  int err;
  int sym;
  int fix;
  int est;
} lotriInfo;

SEXP lotriToLstMat(SEXP lotri);

lotriInfo assertCorrectMatrixProperties(SEXP lst_, SEXP format, SEXP startNum, int *named);
SEXP _lotriLstToMat(SEXP lst_, SEXP format, SEXP startNum);

static inline lotriInfo _lotriLstToMat0(SEXP lst_, SEXP format, SEXP startNum) {
  lotriInfo ret;
  ret.err = 0;
  int pro = 0;
  ret.sym = 0;
  ret.fix = 0;
  ret.est = 0;
  ret.lst = PROTECT(lotriToLstMat(lst_)); pro++;
  int fmtType = TYPEOF(format);
  ret.doFormat = 0;
  if (fmtType == STRSXP && Rf_length(format) == 1) {
    ret.fmt = CHAR(STRING_ELT(format, 0));
    ret.doFormat=1;
  } else if (fmtType) {
    ret.err = 1;
    UNPROTECT(pro);
    return ret;
  } else {
    SEXP fmt2 = Rf_getAttrib(lst_, Rf_install("format"));
    if (TYPEOF(fmt2) == STRSXP && Rf_length(fmt2) == 1) {
      ret.fmt = CHAR(STRING_ELT(fmt2, 0));
      ret.doFormat=1;
    }
  }
  ret.counter = 0;
  if (ret.doFormat) {
    ret.counter = isSingleInt(startNum, NA_INTEGER);
    if (ret.counter == NA_INTEGER){
      SEXP startNum2 = Rf_getAttrib(lst_, Rf_install("start"));
      ret.counter = isSingleInt(startNum2, NA_INTEGER);
      if (ret.counter == NA_INTEGER) {
	ret.err = 2;
	UNPROTECT(pro);
	return ret;
      }
    }
  }
  UNPROTECT(pro);
  return ret;
}

static inline void lotriFillInFixedMatrix (int *reti, int *curBand, int *j, int *totdim,  int *totN, SEXP curFixed) {
  if (!Rf_isNull(curFixed)) {
    int *curi = INTEGER(curFixed);
    memcpy(&reti[(totdim[0])*(curBand[0] + j[0])+(curBand[0])],
	   &curi[totN[0]*j[0]], sizeof(int)*totN[0]);
  }
}

static inline void lotriLstToMatFillInMatrixBand(double *retd, int *retf, int nsame, int type, int named, int totN, int totdim,
						 SEXP retN, SEXP colnames, int *curBand, lotriInfo *li,
						 SEXP cur, SEXP curFixed) {
  for (int cursame = nsame; cursame--;){
    if (type == REALSXP) {
      double *curd = REAL(cur);
      for (int j = 0; j  < totN; ++j) {
	memcpy(&retd[totdim*(*curBand+j)+(*curBand)],
	       &curd[totN*j], sizeof(double)*totN);
	lotriFillInFixedMatrix (retf, curBand, &j, &totdim,  &totN, curFixed);
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
	lotriFillInFixedMatrix (retf, curBand, &j, &totdim,  &totN, curFixed);
	if (named) {
	  setStrElt(retN, colnames, (*curBand), j,
		    li->fmt, li->doFormat, &(li->counter), nsame);
	}
      }
    }
    *curBand += totN;
  }
}

#endif
