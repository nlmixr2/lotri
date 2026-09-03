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
  int fix; // Is there a fixed matrix to store?
  R_xlen_t est; // Stores # of estimated parameters to concatenate (if needed)
} lotriInfo;

SEXP lotriToLstMat(SEXP lotri);

lotriInfo assertCorrectMatrixProperties(SEXP lst_, SEXP format, SEXP startNum, int *named);
SEXP _lotriLstToMat(SEXP lst_, SEXP format, SEXP startNum, SEXP matCls);

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
      SEXP startNum2 = PROTECT(Rf_getAttrib(lst_, Rf_install("start"))); pro++;
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

/* Per-parameter character attributes (`lotriLabels`, `lotriPriors`)
   are stored on each block in the same order as the block's dimnames,
   so they are concatenated with the same banding walk that builds the
   matrix itself.  Without this they would simply be dropped whenever
   blocks are combined. */
static inline int lotriLstHasStrAttr(SEXP lst, R_xlen_t len, const char *what) {
  for (R_xlen_t i = 0; i < len; ++i) {
    SEXP cur = VECTOR_ELT(lst, i);
    if (TYPEOF(cur) == VECSXP) cur = VECTOR_ELT(cur, 0);
    SEXP curAttr = Rf_getAttrib(cur, Rf_install(what));
    if (TYPEOF(curAttr) == STRSXP && Rf_length(curAttr) == Rf_ncols(cur)) {
      return 1;
    }
  }
  return 0;
}

static inline void lotriLstToMatFillInStrAttr(SEXP out, const char *what,
					      SEXP lst, R_xlen_t len) {
  int curBand = 0;
  for (R_xlen_t i = 0; i < len; ++i) {
    SEXP cur = VECTOR_ELT(lst, i);
    int nsame = 1;
    if (TYPEOF(cur) == VECSXP) {
      nsame = isSingleInt(VECTOR_ELT(cur, 1), 1);
      cur = VECTOR_ELT(cur, 0);
    }
    int totN = Rf_ncols(cur);
    SEXP curAttr = PROTECT(Rf_getAttrib(cur, Rf_install(what)));
    int has = (TYPEOF(curAttr) == STRSXP && Rf_length(curAttr) == totN);
    for (int cursame = nsame; cursame--;) {
      for (int j = 0; j < totN; ++j) {
	if (has) {
	  SET_STRING_ELT(out, curBand + j, STRING_ELT(curAttr, j));
	} else {
	  SET_STRING_ELT(out, curBand + j, NA_STRING);
	}
      }
      curBand += totN;
    }
    UNPROTECT(1);
  }
}

/* Attach one concatenated character attribute to the assembled matrix,
   when any of the blocks carried it.  Returns 1 when it was set, so the
   caller knows the result needs the `lotriFix` class. */
static inline int lotriSetStrAttr(SEXP ret, const char *what, SEXP lst,
				  R_xlen_t len, int totdim, int *pro) {
  if (!lotriLstHasStrAttr(lst, len, what)) return 0;
  SEXP out = PROTECT(Rf_allocVector(STRSXP, totdim)); (*pro)++;
  lotriLstToMatFillInStrAttr(out, what, lst, len);
  Rf_setAttrib(ret, Rf_install(what), out);
  return 1;
}

/* `lotriSame` is a per-parameter INTEGER attribute: 0 where the
   parameter is not part of a repeated block, otherwise the distance
   back to the parameter it mirrors.  It has to be carried across the
   concatenation for the same reason the character attributes do, but
   more urgently: dropping it silently turns a repeated block into
   independently estimated parameters, changing the parameter count of
   the model rather than merely losing an annotation.

   The offsets are RELATIVE, so a block that is stamped `nsame` times
   simply gets its own copy of the offsets for each repeat; no
   renumbering is needed. */
static inline int lotriLstHasIntAttr(SEXP lst, R_xlen_t len, const char *what) {
  for (R_xlen_t i = 0; i < len; ++i) {
    SEXP cur = VECTOR_ELT(lst, i);
    if (TYPEOF(cur) == VECSXP) cur = VECTOR_ELT(cur, 0);
    SEXP curAttr = Rf_getAttrib(cur, Rf_install(what));
    /* a hand set attribute can be a double; the R side coerces, so
       accept it here too rather than silently dropping the block's
       repetition on the way through the concatenation */
    if ((TYPEOF(curAttr) == INTSXP || TYPEOF(curAttr) == REALSXP) &&
	Rf_length(curAttr) == Rf_ncols(cur)) {
      return 1;
    }
  }
  return 0;
}

static inline void lotriLstToMatFillInIntAttr(SEXP out, const char *what,
					      SEXP lst, R_xlen_t len) {
  int curBand = 0;
  int *outi = INTEGER(out);
  for (R_xlen_t i = 0; i < len; ++i) {
    SEXP cur = VECTOR_ELT(lst, i);
    int nsame = 1;
    if (TYPEOF(cur) == VECSXP) {
      nsame = isSingleInt(VECTOR_ELT(cur, 1), 1);
      cur = VECTOR_ELT(cur, 0);
    }
    int totN = Rf_ncols(cur);
    SEXP curAttr = PROTECT(Rf_getAttrib(cur, Rf_install(what)));
    int isInt = (TYPEOF(curAttr) == INTSXP);
    int isReal = (TYPEOF(curAttr) == REALSXP);
    int has = ((isInt || isReal) && Rf_length(curAttr) == totN);
    for (int cursame = nsame; cursame--;) {
      for (int j = 0; j < totN; ++j) {
	if (!has) {
	  outi[curBand + j] = 0;
	} else if (isInt) {
	  outi[curBand + j] = INTEGER(curAttr)[j];
	} else {
	  double v = REAL(curAttr)[j];
	  /* only a whole number is an offset; truncating 2.7 to 2 would
	     invent a repetition that was never described */
	  outi[curBand + j] =
	    (ISNA(v) || ISNAN(v) || v != (double)((int)v)) ? 0 : (int)v;
	}
      }
      curBand += totN;
    }
    UNPROTECT(1);
  }
}

/* Attach one concatenated integer attribute to the assembled matrix,
   when any of the blocks carried it.  Returns 1 when it was set, so the
   caller knows the result needs the `lotriFix` class. */
static inline int lotriSetIntAttr(SEXP ret, const char *what, SEXP lst,
				  R_xlen_t len, int totdim, int *pro) {
  if (!lotriLstHasIntAttr(lst, len, what)) return 0;
  SEXP out = PROTECT(Rf_allocVector(INTSXP, totdim)); (*pro)++;
  lotriLstToMatFillInIntAttr(out, what, lst, len);
  Rf_setAttrib(ret, Rf_install(what), out);
  return 1;
}

static inline void lotriLstToMatFillInFullMatrix(double *retd, int *retf, int *totdim, SEXP retN,
						 int *curBand, R_xlen_t *len, lotriInfo *li, int *named) {
  SEXP sameS, dimnames, colnames, curFixed = R_NilValue;
  int totN;
  for (R_xlen_t i = 0; i < *len; ++i) {
    SEXP cur = VECTOR_ELT(li->lst, i);
    int type = TYPEOF(cur);
    int nsame = 1;
    if (type == VECSXP) {
      sameS = VECTOR_ELT(cur, 1);
      nsame = isSingleInt(sameS, 1);
      cur = VECTOR_ELT(cur, 0);
      type = TYPEOF(cur);
    }
    totN = Rf_ncols(cur);
    if (*named) {
      dimnames = Rf_getAttrib(cur, R_DimNamesSymbol);
      colnames = VECTOR_ELT(dimnames, 1);
    }
    int proFix = 0;
    curFixed = R_NilValue;
    if (li->fix) {
      curFixed = PROTECT(Rf_getAttrib(cur, Rf_install("lotriFix"))); proFix++;
      if (!Rf_isMatrix(curFixed) || TYPEOF(curFixed) != LGLSXP) {
	curFixed = R_NilValue;
      }
    }
    lotriLstToMatFillInMatrixBand(retd, retf, nsame, type, *named, totN, *totdim,
				  retN, colnames, curBand, li, cur, curFixed);
    if (proFix) UNPROTECT(proFix);
  }
}

#endif
