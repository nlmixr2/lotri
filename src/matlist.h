#ifndef __MATLIST_H__
#define __MATLIST_H__

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <ctype.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("lotri", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif


static inline int casecmp(const char *s1, const char *s2) {
  register unsigned char u1, u2;
  for (;;) {
    u1 = (unsigned char) tolower(*s1++);
    u2 = (unsigned char) tolower(*s2++);
    if (u1 != u2) {
      return u1 - u2;
    }
    if (u1 == 0 || u2 == 0) {
      return 0;
    }
  }
}

static inline int isSingleInt(SEXP in, int defaultVal) {
  int type = TYPEOF(in);
  if (type == INTSXP && Rf_length(in) == 1) {
    if (!Rf_isMatrix(in)) return INTEGER(in)[0];
  } else if (type == REALSXP && Rf_length(in) == 1) {
    if (!Rf_isMatrix(in)) return (int)(REAL(in)[0]);
  }
  return defaultVal;
}

static inline int isSymNameMat(SEXP cur, int named) {
  int type = TYPEOF(cur);
  if (type == INTSXP || type == REALSXP) {
    if (Rf_isMatrix(cur)){
      int nrows = Rf_nrows(cur);
      int ncols = Rf_ncols(cur);
      if (nrows == ncols) {
	if (!named) return nrows;
	SEXP dimn = Rf_getAttrib(cur, R_DimNamesSymbol);
	if (dimn != R_NilValue) {
	  return nrows;
	}
      }
    }
  }
  return 0;
}

static inline int getCheckDim(SEXP lst, int i, int *named) {
  SEXP cur = VECTOR_ELT(lst, i);
  int type = TYPEOF(cur);
  int same=1;
  if (type == VECSXP) {
    if (Rf_length(cur) != 2){
      Rf_errorcall(R_NilValue, _("when repeating matrices you need to use 'list(mat, n)'"));
    }
    same = isSingleInt(VECTOR_ELT(cur, 1), NA_INTEGER);
    if (same == NA_INTEGER) {
      Rf_errorcall(R_NilValue, _("you can only repeat a matrix a single positive number of times"));
    }
    if (same <= 0) {
      Rf_errorcall(R_NilValue, _("you need to repeat a matrix a positive number of times"));
    }
    cur = VECTOR_ELT(cur, 0);
    type = TYPEOF(cur);
  }
  int ret = isSymNameMat(cur, *named);
  if (ret){
    return ret*same;
  } else {
    // if named is 2, then reassign named to 0 and return the dimension, reset the named to 0
    if (*named == 2) {
      ret = isSymNameMat(cur, 0);
      if (ret) {
	*named = 0;
	return ret*same;
      }
    }
    if (*named) Rf_errorcall(R_NilValue, _("list element %d is not a symmetric named matrix"), i+1);
    else Rf_errorcall(R_NilValue, _("list element %d is not a symmetric matrix"), i+1);
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

static inline double getDouble(SEXP colnames, int i, SEXP inUpperLower, SEXP upperLowerNames,
			       double defaultValue, int type) {
  const char *lookup = CHAR(STRING_ELT(colnames, i));
  const char *current;
  int upperLowerNamesSize = Rf_length(upperLowerNames);
  int inUpperLowerSize = Rf_length(inUpperLower);
  if (inUpperLowerSize != upperLowerNamesSize) {
    Rf_errorcall(R_NilValue,_("malformed upper/lower names; names length and vector length are unequal"));
  }
  for (int j = Rf_length(upperLowerNames); j--;) {
    current = CHAR(STRING_ELT(upperLowerNames, j));
    if (!strcmp(current, lookup)){
      return REAL(inUpperLower)[j];
    }
  }
  return defaultValue;
}

static inline int setUpperLower(SEXP inUpperLower, SEXP colnames,
				double *outUpperLower, int i0, double defaultValue,
				const char *what, int nsame) {
  SEXP upperLowerNames = Rf_getAttrib(inUpperLower, R_NamesSymbol);
  double value = defaultValue;
  int ncol = Rf_length(colnames);
  if (Rf_isNull(upperLowerNames)){
    if (Rf_length(inUpperLower) == 1) {
      int typ = TYPEOF(inUpperLower);
      if (typ == REALSXP) {
	value = REAL(inUpperLower)[0];
      } else if (typ == INTSXP) {
	value = (double)(INTEGER(inUpperLower)[0]);
      }
    } else if (Rf_length(inUpperLower) != 0) {
      /* UNPROTECT(*pro0); */
      /* Rf_errorcall(R_NilValue, _("cannot figure out valid '%s' properties"), what); */
      return 1;
    }
    for (int i = ncol*nsame; i--;) {
      outUpperLower[i0+i] = value;
    }
  } else {
    int typ = TYPEOF(inUpperLower);
    for (int i = ncol; i--;) {
      outUpperLower[i0+i] = getDouble(colnames, i, inUpperLower,
				      upperLowerNames, defaultValue, typ);
    }
    for (int i = 1; i < nsame; ++i) {
      memcpy(&outUpperLower[i0+i*ncol], &outUpperLower[i0], ncol*sizeof(double));
    }
  }
  return 0;
}



#include "asLotriMat.h"
#include "lotriProp.h"
#include "lotriBounds.h"
#include "lotriLstToMat.h"
#include "lotriNest.h"
#include "matlist.h"


#ifndef HAVE_STRCASECMP
#define HAVE_STRCASECMP 0
#endif



SEXP getLotriProp(SEXP names, int i, SEXP lotriProp, SEXP lotriPropNames, const char *prop);

SEXP lotriToLstMat(SEXP lotri);

SEXP _lotriAllNames(SEXP lotri);

#endif // __MATLIST_H__
