#ifndef LOTRI_PROP_H_
#define LOTRI_PROP_H_

#include <cstring>
#include <string_view>

namespace lotri {

// Get a property from the lotri property list
inline SEXP getLotriProperty(SEXP names, int i, SEXP lotriProp,
                             SEXP lotriPropNames, std::string_view prop) {
  const char *what = CHAR(STRING_ELT(names, i));
  const int propCount = Rf_length(lotriPropNames);

  for (int j = propCount - 1; j >= 0; --j) {
    const char *cur = CHAR(STRING_ELT(lotriPropNames, j));
    if (std::strcmp(what, cur) != 0)
      continue;

    SEXP lotriCur = VECTOR_ELT(lotriProp, j);
    SEXP lotriCurNames = Rf_getAttrib(lotriCur, R_NamesSymbol);
    const int curLen = Rf_length(lotriCurNames);

    for (int k = curLen - 1; k >= 0; --k) {
      const char *cur2 = CHAR(STRING_ELT(lotriCurNames, k));
      if (prop == cur2) {
        return VECTOR_ELT(lotriCur, k);
      }
    }
  }

  return R_NilValue;
}

// Create a blank property list with given names
inline SEXP createBlankPropertyList(SEXP names) {
  const int len = Rf_length(names);
  SEXP lotriProp = PROTECT(Rf_allocVector(VECSXP, len));

  for (int j = len - 1; j >= 0; --j) {
    SET_VECTOR_ELT(lotriProp, j, Rf_allocVector(VECSXP, 0));
  }

  Rf_setAttrib(lotriProp, R_NamesSymbol, names);
  UNPROTECT(1);
  return lotriProp;
}

// Get the 'same' property value (for repeated matrices)
inline int getSameValue(SEXP names, int i, SEXP lotriProp,
                        SEXP lotriPropNames) {
  SEXP s = getLotriProperty(names, i, lotriProp, lotriPropNames, "same");
  if (!Rf_isNull(s)) {
    return getSingleInt(s, 1);
  }
  return 1;
}

} // namespace lotri

// Legacy compatibility wrappers
static inline SEXP getLotriProp(SEXP names, int i, SEXP lotriProp,
                                SEXP lotriPropNames, const char *prop) {
  return lotri::getLotriProperty(names, i, lotriProp, lotriPropNames, prop);
}

static inline SEXP blankProp(SEXP names) {
  return lotri::createBlankPropertyList(names);
}

static inline int getSame(SEXP names, int i, SEXP lotriProp,
                          SEXP lotriPropNames) {
  return lotri::getSameValue(names, i, lotriProp, lotriPropNames);
}

/* roxygen
@title Maximum nu from lotri Object
@description Get the maximum nu value from a lotri matrix object.
@param lotri A lotri object
@return Maximum nu value as a double
@keywords internal
*/
[[cpp4r::register]] double lotriMaxNu(SEXP lotri) {
  SEXP lotriProp = Rf_getAttrib(lotri, Rf_install("lotri"));
  if (Rf_isNull(lotriProp)) {
    return 0.0;
  }

  SEXP lotriPropNames = Rf_getAttrib(lotriProp, R_NamesSymbol);
  SEXP names = Rf_getAttrib(lotri, R_NamesSymbol);
  double maxNu = 0.0;

  const int len = Rf_length(lotri);
  for (int i = len - 1; i >= 0; --i) {
    SEXP nu =
        lotri::getLotriProperty(names, i, lotriProp, lotriPropNames, "nu");
    if (!Rf_isNull(nu) && Rf_length(nu) == 1 && TYPEOF(nu) == REALSXP) {
      double tmp = REAL(nu)[0];
      if (tmp > maxNu) {
        maxNu = tmp;
      }
    }
  }

  return maxNu;
}

namespace lotri {

// Add a property to the end of an existing property list
inline SEXP addPropertyAtEnd(SEXP lotri0, int i, SEXP sameC, int *nestI,
                             int extra) {
  SEXP curProp = VECTOR_ELT(lotri0, i);
  const int curPropN = Rf_length(curProp);
  SEXP curPropS = PROTECT(Rf_getAttrib(curProp, R_NamesSymbol));

  SEXP newProp = PROTECT(Rf_allocVector(VECSXP, curPropN + 1));
  SEXP newPropS = PROTECT(Rf_allocVector(STRSXP, curPropN + 1));

  // Copy existing properties
  for (int k = 0; k < curPropN; ++k) {
    SET_VECTOR_ELT(newProp, k, VECTOR_ELT(curProp, k));
    SET_STRING_ELT(newPropS, k, STRING_ELT(curPropS, k));
  }

  // Add new "same" property
  SET_STRING_ELT(newPropS, curPropN, sameC);
  SEXP nestVal = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(nestVal)[0] = nestI[i - extra];
  SET_VECTOR_ELT(newProp, curPropN, nestVal);

  Rf_setAttrib(newProp, R_NamesSymbol, newPropS);
  UNPROTECT(4);
  return newProp;
}

// Amplify property with default values for all dimensions
inline SEXP amplifyPropertyWithDefault(SEXP cur, SEXP dimn, double defaultVal,
                                       int proOuter, const char *what) {
  if (TYPEOF(cur) != REALSXP) {
    UNPROTECT(proOuter);
    Rf_errorcall(R_NilValue, "'%s' needs to be a double", what);
  }

  const R_xlen_t nDim = Rf_xlength(dimn);
  SEXP names = Rf_getAttrib(cur, R_NamesSymbol);

  SEXP ret = PROTECT(Rf_allocVector(REALSXP, nDim));
  double *retD = REAL(ret);

  if (Rf_isNull(names)) {
    if (Rf_xlength(cur) == 1) {
      const double valIn = REAL(cur)[0];
      std::fill_n(retD, nDim, valIn);
      Rf_setAttrib(ret, R_NamesSymbol, dimn);
      UNPROTECT(1);
      return ret;
    } else {
      UNPROTECT(1);
      UNPROTECT(proOuter);
      Rf_errorcall(R_NilValue, "'%s' needs to be named", what);
    }
  }

  // Named vector: match names to dimensions
  const R_xlen_t nNames = Rf_xlength(names);
  const double *in = REAL(cur);

  for (R_xlen_t i = 0; i < nDim; ++i) {
    const char *dimName = CHAR(STRING_ELT(dimn, i));
    bool found = false;

    for (R_xlen_t j = 0; j < nNames; ++j) {
      if (std::strcmp(dimName, CHAR(STRING_ELT(names, j))) == 0) {
        retD[i] = in[j];
        found = true;
        break;
      }
    }

    if (!found) {
      retD[i] = defaultVal;
    }
  }

  Rf_setAttrib(ret, R_NamesSymbol, dimn);
  UNPROTECT(1);
  return ret;
}

} // namespace lotri

// Legacy compatibility wrapper
static inline SEXP addLotriPropertyAtEnd(SEXP lotri0, int i, SEXP sameC,
                                         int *nestI, int extra) {
  return lotri::addPropertyAtEnd(lotri0, i, sameC, nestI, extra);
}

static inline SEXP lotriPropAmpDefault(SEXP cur, SEXP dimn, double val,
                                       int pro0, const char *what) {
  return lotri::amplifyPropertyWithDefault(cur, dimn, val, pro0, what);
}

#endif // LOTRI_PROP_H_
