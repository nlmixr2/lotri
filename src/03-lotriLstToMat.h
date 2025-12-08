#ifndef LOTRI_LSTTOMAT_H_
#define LOTRI_LSTTOMAT_H_

#include <algorithm>
#include <cstring>

namespace lotri {

// Configuration and result structure for list-to-matrix conversion
struct LotriConversionInfo {
  SEXP lst{R_NilValue};
  bool doFormat{false};
  const char *fmt{nullptr};
  int counter{0};
  int err{0};
  bool isSymmetric{false};
  bool hasFixed{false};
  int estimateCount{0};
};

// Convert lotri object to list of matrices (expands "same" properties)
inline SEXP expandLotriToList(SEXP lotri) {
  SEXP lotriProp = Rf_getAttrib(lotri, Rf_install("lotri"));
  if (Rf_isNull(lotriProp)) {
    return lotri;
  }

  SEXP lotriNames = Rf_getAttrib(lotri, R_NamesSymbol);
  SEXP lotriPropNames = Rf_getAttrib(lotriProp, R_NamesSymbol);

  const int len = Rf_length(lotri);
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, len));

  for (int i = len - 1; i >= 0; --i) {
    int nsame = getSameValue(lotriNames, i, lotriProp, lotriPropNames);

    if (nsame > 1) {
      SEXP cur = PROTECT(Rf_allocVector(VECSXP, 2));
      SET_VECTOR_ELT(cur, 0, VECTOR_ELT(lotri, i));

      SEXP ns = PROTECT(Rf_allocVector(INTSXP, 1));
      INTEGER(ns)[0] = nsame;
      SET_VECTOR_ELT(cur, 1, ns);

      SET_VECTOR_ELT(ret, i, cur);
      UNPROTECT(2);
    } else {
      SET_VECTOR_ELT(ret, i, VECTOR_ELT(lotri, i));
    }
  }

  UNPROTECT(1);
  return ret;
}

// Initialize conversion info from list
inline LotriConversionInfo initConversionInfo(SEXP lst_, SEXP format,
                                              SEXP startNum) {
  LotriConversionInfo info;

  SEXP expanded = PROTECT(expandLotriToList(lst_));
  info.lst = expanded;
  UNPROTECT(1);

  // Check format parameter
  const int fmtType = TYPEOF(format);
  if (fmtType == STRSXP && Rf_length(format) == 1) {
    info.fmt = CHAR(STRING_ELT(format, 0));
    info.doFormat = true;
  } else if (fmtType != 0 && fmtType != NILSXP) {
    info.err = 1;
    return info;
  } else {
    // Check for format attribute on the list
    SEXP fmt2 = Rf_getAttrib(lst_, Rf_install("format"));
    if (TYPEOF(fmt2) == STRSXP && Rf_length(fmt2) == 1) {
      info.fmt = CHAR(STRING_ELT(fmt2, 0));
      info.doFormat = true;
    }
  }

  // Get starting counter
  if (info.doFormat) {
    info.counter = getSingleInt(startNum, NA_INTEGER);
    if (info.counter == NA_INTEGER) {
      SEXP startNum2 = Rf_getAttrib(lst_, Rf_install("start"));
      info.counter = getSingleInt(startNum2, NA_INTEGER);
      if (info.counter == NA_INTEGER) {
        info.err = 2;
        return info;
      }
    }
  }

  return info;
}

// Validate matrix properties and return conversion info
inline LotriConversionInfo validateMatrixProperties(SEXP lst_, SEXP format,
                                                    SEXP startNum, int *named) {
  const int type = TYPEOF(lst_);

  if (type != VECSXP) {
    int fixed = 0;
    int estimate = 0;
    if (isSymNameMat(lst_, *named, &fixed, &estimate)) {
      LotriConversionInfo info;
      info.isSymmetric = true;
      info.hasFixed = (fixed != 0);
      info.estimateCount = estimate;
      return info;
    }
    Rf_errorcall(R_NilValue, _("expects a list named symmetric matrices"));
  }

  LotriConversionInfo info = initConversionInfo(lst_, format, startNum);
  PROTECT(info.lst);

  if (info.err == 1) {
    UNPROTECT(1);
    Rf_errorcall(R_NilValue,
                 _("'format' must be a single length string or NULL"));
  }
  if (info.err == 2) {
    UNPROTECT(1);
    Rf_errorcall(
        R_NilValue,
        _("when format is specified, 'startNum' must be a single integer"));
  }

  UNPROTECT(1);
  return info;
}

// Fill in fixed matrix data
inline void fillFixedMatrix(int *reti, int curBand, int j, int totdim, int totN,
                            SEXP curFixed) {
  if (!Rf_isNull(curFixed)) {
    const int *curi = INTEGER(curFixed);
    std::memcpy(&reti[totdim * (curBand + j) + curBand], &curi[totN * j],
                sizeof(int) * totN);
  }
}

// Fill in a single matrix band
inline void fillMatrixBand(double *retd, int *retf, int nsame, int type,
                           bool named, int totN, int totdim, SEXP retN,
                           SEXP colnames, int *curBand,
                           LotriConversionInfo *info, SEXP cur, SEXP curFixed) {
  for (int cursame = nsame; cursame-- > 0;) {
    if (type == REALSXP) {
      const double *curd = REAL(cur);
      for (int j = 0; j < totN; ++j) {
        std::memcpy(&retd[totdim * (*curBand + j) + (*curBand)],
                    &curd[totN * j], sizeof(double) * totN);
        fillFixedMatrix(retf, *curBand, j, totdim, totN, curFixed);

        if (named) {
          setStrElt(retN, colnames, *curBand, j, info->fmt, info->doFormat,
                    &info->counter, nsame);
        }
      }
    } else {
      const int *curi = INTEGER(cur);
      for (int j = 0; j < totN; ++j) {
        double *to = &retd[totdim * (*curBand + j) + (*curBand)];
        const int *from = &curi[totN * j];

        for (int k = 0; k < totN; ++k) {
          to[k] = static_cast<double>(from[k]);
        }

        fillFixedMatrix(retf, *curBand, j, totdim, totN, curFixed);

        if (named) {
          setStrElt(retN, colnames, *curBand, j, info->fmt, info->doFormat,
                    &info->counter, nsame);
        }
      }
    }
    *curBand += totN;
  }
}

// Fill in the full block-diagonal matrix
inline void fillFullMatrix(double *retd, int *retf, int totdim, SEXP retN,
                           int *curBand, int len, LotriConversionInfo *info,
                           bool named) {
  for (int i = 0; i < len; ++i) {
    SEXP cur = VECTOR_ELT(info->lst, i);
    int type = TYPEOF(cur);
    int nsame = 1;

    if (type == VECSXP) {
      SEXP sameS = VECTOR_ELT(cur, 1);
      nsame = getSingleInt(sameS, 1);
      cur = VECTOR_ELT(cur, 0);
      type = TYPEOF(cur);
    }

    const int totN = Rf_ncols(cur);
    SEXP colnames = R_NilValue;

    if (named) {
      SEXP dimnames = Rf_getAttrib(cur, R_DimNamesSymbol);
      if (!Rf_isNull(dimnames)) {
        colnames = VECTOR_ELT(dimnames, 1);
      }
    }

    SEXP curFixed = R_NilValue;
    if (info->hasFixed) {
      curFixed = Rf_getAttrib(cur, Rf_install("lotriFix"));
      if (!Rf_isMatrix(curFixed) || TYPEOF(curFixed) != LGLSXP) {
        curFixed = R_NilValue;
      }
    }

    fillMatrixBand(retd, retf, nsame, type, named, totN, totdim, retN, colnames,
                   curBand, info, cur, curFixed);
  }
}

} // namespace lotri

// Legacy compatibility wrapper
static inline SEXP lotriToLstMat(SEXP lotri) {
  return lotri::expandLotriToList(lotri);
}

// Legacy struct for backward compatibility
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

// Legacy wrapper functions
static inline lotriInfo lotriLstToMat0(SEXP lst_, SEXP format, SEXP startNum) {
  lotri::LotriConversionInfo info =
      lotri::initConversionInfo(lst_, format, startNum);

  lotriInfo ret;
  ret.lst = info.lst;
  ret.doFormat = info.doFormat ? 1 : 0;
  ret.fmt = info.fmt;
  ret.counter = info.counter;
  ret.err = info.err;
  ret.sym = info.isSymmetric ? 1 : 0;
  ret.fix = info.hasFixed ? 1 : 0;
  ret.est = info.estimateCount;

  return ret;
}

static inline lotriInfo _lotriLstToMat0(SEXP lst_, SEXP format, SEXP startNum) {
  return lotriLstToMat0(lst_, format, startNum);
}

static inline lotriInfo assertCorrectMatrixProperties(SEXP lst_, SEXP format,
                                                      SEXP startNum,
                                                      int *named) {
  const int type = TYPEOF(lst_);

  if (type != VECSXP) {
    int fixed = 0;
    int estimate = 0;
    if (isSymNameMat(lst_, *named, &fixed, &estimate)) {
      lotriInfo li;
      li.sym = 1;
      li.lst = R_NilValue;
      li.fix = fixed;
      li.est = estimate;
      li.doFormat = 0;
      li.fmt = nullptr;
      li.counter = 0;
      li.err = 0;
      return li;
    }
    Rf_errorcall(R_NilValue, _("expects a list named symmetric matrices"));
  }

  lotriInfo li = lotriLstToMat0(lst_, format, startNum);
  PROTECT(li.lst);

  if (li.err == 1) {
    UNPROTECT(1);
    Rf_errorcall(R_NilValue,
                 _("'format' must be a single length string or NULL"));
  }
  if (li.err == 2) {
    UNPROTECT(1);
    Rf_errorcall(
        R_NilValue,
        _("when format is specified, 'startNum' must be a single integer"));
  }

  UNPROTECT(1);
  return li;
}

static inline void lotriFillInFixedMatrix(int *reti, int *curBand, int *j,
                                          int *totdim, int *totN,
                                          SEXP curFixed) {
  if (!Rf_isNull(curFixed)) {
    const int *curi = INTEGER(curFixed);
    std::memcpy(&reti[(*totdim) * (*curBand + *j) + (*curBand)],
                &curi[(*totN) * (*j)], sizeof(int) * (*totN));
  }
}

static inline void lotriLstToMatFillInMatrixBand(double *retd, int *retf,
                                                 int nsame, int type, int named,
                                                 int totN, int totdim,
                                                 SEXP retN, SEXP colnames,
                                                 int *curBand, lotriInfo *li,
                                                 SEXP cur, SEXP curFixed) {
  for (int cursame = nsame; cursame-- > 0;) {
    if (type == REALSXP) {
      const double *curd = REAL(cur);
      for (int j = 0; j < totN; ++j) {
        std::memcpy(&retd[totdim * (*curBand + j) + (*curBand)],
                    &curd[totN * j], sizeof(double) * totN);
        lotriFillInFixedMatrix(retf, curBand, &j, &totdim, &totN, curFixed);
        if (named) {
          setStrElt(retN, colnames, *curBand, j, li->fmt, li->doFormat != 0,
                    &li->counter, nsame);
        }
      }
    } else {
      const int *curi = INTEGER(cur);
      for (int j = 0; j < totN; ++j) {
        double *to = &retd[totdim * (*curBand + j) + (*curBand)];
        const int *from = &curi[totN * j];
        for (int k = 0; k < totN; ++k) {
          to[k] = static_cast<double>(from[k]);
        }
        lotriFillInFixedMatrix(retf, curBand, &j, &totdim, &totN, curFixed);
        if (named) {
          setStrElt(retN, colnames, *curBand, j, li->fmt, li->doFormat != 0,
                    &li->counter, nsame);
        }
      }
    }
    *curBand += totN;
  }
}

static inline void lotriLstToMatFillInFullMatrix(double *retd, int *retf,
                                                 int *totdim, SEXP retN,
                                                 int *curBand, int *len,
                                                 lotriInfo *li, int *named) {
  for (int i = 0; i < *len; ++i) {
    SEXP cur = VECTOR_ELT(li->lst, i);
    int type = TYPEOF(cur);
    int nsame = 1;

    if (type == VECSXP) {
      SEXP sameS = VECTOR_ELT(cur, 1);
      nsame = lotri::getSingleInt(sameS, 1);
      cur = VECTOR_ELT(cur, 0);
      type = TYPEOF(cur);
    }

    const int totN = Rf_ncols(cur);
    SEXP colnames = R_NilValue;

    if (*named) {
      SEXP dimnames = Rf_getAttrib(cur, R_DimNamesSymbol);
      if (!Rf_isNull(dimnames)) {
        colnames = VECTOR_ELT(dimnames, 1);
      }
    }

    SEXP curFixed = R_NilValue;
    if (li->fix) {
      curFixed = Rf_getAttrib(cur, Rf_install("lotriFix"));
      if (!Rf_isMatrix(curFixed) || TYPEOF(curFixed) != LGLSXP) {
        curFixed = R_NilValue;
      }
    }

    lotriLstToMatFillInMatrixBand(retd, retf, nsame, type, *named, totN,
                                  *totdim, retN, colnames, curBand, li, cur,
                                  curFixed);
  }
}

static inline SEXP lotriEstDf(SEXP lst_, int totNum) {
  const int lstLen = Rf_length(lst_);

  // Create the data frame structure
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 7));
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 7));

  // Column names
  const char *colNames[] = {"name", "lower", "est",          "upper",
                            "fix",  "label", "backTransform"};
  for (int i = 0; i < 7; ++i) {
    SET_STRING_ELT(retN, i, Rf_mkChar(colNames[i]));
  }

  // Allocate columns
  SEXP name = PROTECT(Rf_allocVector(STRSXP, totNum));
  SEXP lowerS = PROTECT(Rf_allocVector(REALSXP, totNum));
  SEXP estS = PROTECT(Rf_allocVector(REALSXP, totNum));
  SEXP upperS = PROTECT(Rf_allocVector(REALSXP, totNum));
  SEXP fixS = PROTECT(Rf_allocVector(LGLSXP, totNum));
  SEXP label = PROTECT(Rf_allocVector(STRSXP, totNum));
  SEXP backTransform = PROTECT(Rf_allocVector(STRSXP, totNum));

  SET_VECTOR_ELT(ret, 0, name);
  SET_VECTOR_ELT(ret, 1, lowerS);
  SET_VECTOR_ELT(ret, 2, estS);
  SET_VECTOR_ELT(ret, 3, upperS);
  SET_VECTOR_ELT(ret, 4, fixS);
  SET_VECTOR_ELT(ret, 5, label);
  SET_VECTOR_ELT(ret, 6, backTransform);

  double *lower = REAL(lowerS);
  double *est = REAL(estS);
  double *upper = REAL(upperS);
  int *fix = INTEGER(fixS);

  // Fill in data
  int i0 = 0;
  for (int listCnt = 0; listCnt < lstLen; ++listCnt) {
    SEXP curV = Rf_getAttrib(VECTOR_ELT(lst_, listCnt), Rf_install("lotriEst"));
    if (Rf_isNull(curV))
      continue;

    SEXP nameIn = VECTOR_ELT(curV, 0);
    const double *lowerIn = REAL(VECTOR_ELT(curV, 1));
    const double *estIn = REAL(VECTOR_ELT(curV, 2));
    const double *upperIn = REAL(VECTOR_ELT(curV, 3));
    const int *fixIn = INTEGER(VECTOR_ELT(curV, 4));
    SEXP labelIn = VECTOR_ELT(curV, 5);
    SEXP backTransformIn = VECTOR_ELT(curV, 6);

    const int inLen = Rf_length(nameIn);
    for (int inCnt = 0; inCnt < inLen; ++inCnt, ++i0) {
      SET_STRING_ELT(name, i0, STRING_ELT(nameIn, inCnt));
      lower[i0] = lowerIn[inCnt];
      est[i0] = estIn[inCnt];
      upper[i0] = upperIn[inCnt];
      fix[i0] = fixIn[inCnt];
      SET_STRING_ELT(label, i0, STRING_ELT(labelIn, inCnt));
      SET_STRING_ELT(backTransform, i0, STRING_ELT(backTransformIn, inCnt));
    }
  }

  // Set data.frame class
  SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(cls, 0, Rf_mkChar("data.frame"));
  Rf_classgets(ret, cls);

  // Set row.names attribute (compact form)
  SEXP rowNamesS = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rowNamesS)[0] = NA_INTEGER;
  INTEGER(rowNamesS)[1] = totNum;

  Rf_setAttrib(ret, R_NamesSymbol, retN);
  Rf_setAttrib(ret, Rf_install("row.names"), rowNamesS);

  UNPROTECT(11);
  return ret;
}

/* roxygen
@title Convert List to Matrix
@description Convert a list of matrices to a single block-diagonal matrix.
@param lst_ A list of matrices
@param format Format string for naming
@param startNum Starting number for formatted names
@param matCls Matrix class
@return A block-diagonal matrix
@keywords internal
*/
[[cpp4r::register]] SEXP lotriLstToMat(SEXP lst_, SEXP format, SEXP startNum,
                                       SEXP matCls) {
  int named = 2;
  lotriInfo li = assertCorrectMatrixProperties(lst_, format, startNum, &named);

  // If already a symmetric matrix, return as-is
  if (li.sym)
    return lst_;

  SEXP protectedLst = PROTECT(li.lst);
  const int len = Rf_length(protectedLst);

  // Handle special case: list(mat, n) for repetition
  if (len == 2) {
    int repN = lotri::getSingleInt(VECTOR_ELT(protectedLst, 1), NA_INTEGER);
    if (repN != NA_INTEGER && repN > 0) {
      if (isSymNameMat(VECTOR_ELT(protectedLst, 0), named, &li.fix, &li.est)) {
        SEXP newLst = PROTECT(Rf_allocVector(VECSXP, 1));
        SET_VECTOR_ELT(newLst, 0, protectedLst);
        SEXP ret = lotriLstToMat(newLst, format, startNum, matCls);
        UNPROTECT(2);
        return ret;
      }
    }
  }

  // Calculate total dimensions
  li.est = 0;
  int totdim = 0;
  for (int i = 0; i < len; ++i) {
    totdim += getCheckDim(protectedLst, i, &named, &li.fix, &li.est);
  }
  const int liEst = li.est;

  // Allocate result matrix
  SEXP ret = PROTECT(Rf_allocMatrix(REALSXP, totdim, totdim));
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, totdim));
  double *retd = REAL(ret);

  // Initialize to zero
  std::memset(retd, 0, sizeof(double) * totdim * totdim);

  // Handle fixed matrix if needed
  SEXP retF = R_NilValue;
  int *retf = nullptr;
  if (li.fix) {
    retF = PROTECT(Rf_allocMatrix(LGLSXP, totdim, totdim));
    retf = INTEGER(retF);
    std::memset(retf, 0, sizeof(int) * totdim * totdim);
  }

  // Fill in the block-diagonal matrix
  int curBand = 0;
  lotriLstToMatFillInFullMatrix(retd, retf, &totdim, retN, &curBand,
                                const_cast<int *>(&len), &li, &named);

  // Set dimension names
  if (named) {
    SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 0, retN);
    SET_VECTOR_ELT(dimnames, 1, retN);
    Rf_setAttrib(ret, R_DimNamesSymbol, dimnames);
    if (li.fix) {
      Rf_setAttrib(retF, R_DimNamesSymbol, dimnames);
    }
    UNPROTECT(1);
  }

  // Set class and additional attributes
  bool doCls = false;
  if (li.fix) {
    doCls = true;
    Rf_setAttrib(ret, Rf_install("lotriFix"), retF);
  }

  if (liEst > 0) {
    doCls = true;
    SEXP liEstSEXP = PROTECT(lotriEstDf(lst_, liEst));
    Rf_setAttrib(ret, Rf_install("lotriEst"), liEstSEXP);
    UNPROTECT(1);
  }

  if (doCls) {
    const int lenCls = Rf_length(matCls);
    SEXP cls = PROTECT(Rf_allocVector(STRSXP, lenCls + 1));
    SET_STRING_ELT(cls, 0, Rf_mkChar("lotriFix"));
    for (int mi = lenCls - 1; mi >= 0; --mi) {
      SET_STRING_ELT(cls, mi + 1, STRING_ELT(matCls, mi));
    }
    Rf_classgets(ret, cls);
    UNPROTECT(1);
  }

  UNPROTECT(li.fix ? 4 : 3);
  return ret;
}

#endif // LOTRI_LSTTOMAT_H_
