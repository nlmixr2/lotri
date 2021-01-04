#ifndef __lotriNest_H__
#define __lotriNest_H__

typedef struct lotriNestInfo {
  SEXP ret;
  int err;
} lotriNestInfo;

typedef struct lotriNestGet {
  int lenNest;
  int extra;
  int lotriLen;
  SEXP nestN;
  SEXP lotri;
  SEXP names;
  SEXP lotri0;
  SEXP lotri0names;
  SEXP sameC;
  int *nestI;
  SEXP nestStart;
} lotriNestGet;

SEXP nestLotriExpandById(lotriNestGet *ng, SEXP nestLotri, SEXP nestLotriProp, lotriNestInfo *ret);
lotriNestInfo getNestLotri(int lenNest, int extra, int lotriLen,
			   SEXP nestN, SEXP lotri, SEXP names, SEXP lotri0, SEXP lotri0names,
			   SEXP sameC, int *nestI, SEXP nestStart);

#endif // lotriNest.h
