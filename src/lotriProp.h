#ifndef __lotriProp_H__
#define __lotriProp_H__

SEXP getLotriProp(SEXP names, int i,
		  SEXP lotriProp,
		  SEXP lotriPropNames, const char *prop);

SEXP blankProp(SEXP names);

SEXP _lotriMaxNu(SEXP lotri);

SEXP addLotriPropertyAtEnd(SEXP lotri0, int i, SEXP sameC, int *nestI, int extra);

SEXP ampDefault(SEXP cur, SEXP dimn, double val, int pro0, const char * what);

static inline int getSame(SEXP names, int i, SEXP lotriProp, SEXP lotriPropNames) {
  SEXP s = getLotriProp(names, i, lotriProp, lotriPropNames, "same");
  if (!Rf_isNull(s)) {
    return isSingleInt(s, 1);
  }
  return 1;
}

#endif
