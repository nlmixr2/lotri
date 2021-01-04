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
} lotriInfo;

SEXP lotriToLstMat(SEXP lotri);

lotriInfo assertCorrectMatrixProperties(SEXP lst_, SEXP format, SEXP startNum);
SEXP _lotriLstToMat(SEXP lst_, SEXP format, SEXP startNum);

static inline lotriInfo _lotriLstToMat0(SEXP lst_, SEXP format, SEXP startNum) {
  lotriInfo ret;
  ret.err = 0;
  int pro = 0;
  ret.sym = 0;
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

#endif
