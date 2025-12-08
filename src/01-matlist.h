#ifndef LOTRI_MATLIST_H_
#define LOTRI_MATLIST_H_

// Core lotri header - must be included first before module-specific headers
// This header provides the base R integration and common utilities

#define R_NO_REMAP
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include <algorithm>
#include <cctype>
#include <cstring>
#include <string>
#include <string_view>

// Internationalization support (currently disabled)
#define _(String) (String)

namespace lotri {

// Case-insensitive string comparison using C++ idioms
inline int casecmp(std::string_view s1, std::string_view s2) noexcept {
  auto it1 = s1.begin();
  auto it2 = s2.begin();

  while (it1 != s1.end() && it2 != s2.end()) {
    const auto c1 = static_cast<unsigned char>(
        std::tolower(static_cast<unsigned char>(*it1++)));
    const auto c2 = static_cast<unsigned char>(
        std::tolower(static_cast<unsigned char>(*it2++)));
    if (c1 != c2) {
      return c1 - c2;
    }
  }

  // Handle different lengths
  if (it1 != s1.end())
    return 1;
  if (it2 != s2.end())
    return -1;
  return 0;
}

} // namespace lotri

namespace lotri {

// Check if SEXP is a single integer and return its value
inline int getSingleInt(SEXP in, int defaultVal) noexcept {
  const int type = TYPEOF(in);
  if (type == INTSXP && Rf_length(in) == 1 && !Rf_isMatrix(in)) {
    return INTEGER(in)[0];
  }
  if (type == REALSXP && Rf_length(in) == 1 && !Rf_isMatrix(in)) {
    return static_cast<int>(REAL(in)[0]);
  }
  return defaultVal;
}

// Matrix validation result
struct MatrixInfo {
  int nrows{0};
  int fixedCount{0};
  int estimateCount{0};
  bool isValid{false};
};

// Returns matrix info if the matrix is a symmetric (named) matrix
inline MatrixInfo checkSymmetricNamedMatrix(SEXP cur,
                                            bool requireNamed) noexcept {
  MatrixInfo info;
  const int type = TYPEOF(cur);

  if (type != INTSXP && type != REALSXP) {
    return info;
  }

  if (!Rf_isMatrix(cur)) {
    return info;
  }

  const int nrows = Rf_nrows(cur);
  const int ncols = Rf_ncols(cur);

  if (nrows != ncols) {
    return info;
  }

  // Check for fixed attribute
  SEXP hasFix = Rf_getAttrib(cur, Rf_install("lotriFix"));
  if (TYPEOF(hasFix) == LGLSXP && Rf_isMatrix(hasFix)) {
    if (Rf_nrows(hasFix) == nrows && Rf_ncols(hasFix) == nrows) {
      info.fixedCount = 1;
    }
  }

  // Check for estimate attribute
  SEXP hasEst = Rf_getAttrib(cur, Rf_install("lotriEst"));
  if (TYPEOF(hasEst) == VECSXP) {
    SEXP estNames = VECTOR_ELT(hasEst, 0);
    info.estimateCount = Rf_length(estNames);
  }

  if (!requireNamed) {
    info.nrows = nrows;
    info.isValid = true;
    return info;
  }

  SEXP dimn = Rf_getAttrib(cur, R_DimNamesSymbol);
  if (dimn != R_NilValue) {
    info.nrows = nrows;
    info.isValid = true;
  }

  return info;
}

} // namespace lotri

// Legacy compatibility wrapper
static inline int isSingleInt(SEXP in, int defaultVal) {
  return lotri::getSingleInt(in, defaultVal);
}

// Returns number of rows if the matrix is a symmetric (named) matrix
// Legacy wrapper - updates fixed/estimate counts via pointers
static inline int isSymNameMat(SEXP cur, int named, int *fixed, int *estimate) {
  auto info = lotri::checkSymmetricNamedMatrix(cur, named != 0);
  if (info.isValid) {
    if (info.fixedCount > 0)
      *fixed = 1;
    *estimate += info.estimateCount;
    return info.nrows;
  }

  // Fallback for unnamed check when named == 2
  if (named == 2) {
    auto info2 = lotri::checkSymmetricNamedMatrix(cur, false);
    if (info2.isValid) {
      if (info2.fixedCount > 0)
        *fixed = 1;
      *estimate += info2.estimateCount;
      return info2.nrows;
    }
  }

  return 0;
}

// Check dimension and validate matrix
static inline int getCheckDim(SEXP lst, int i, int *named, int *fixed,
                              int *estimate) {
  SEXP cur = VECTOR_ELT(lst, i);
  int type = TYPEOF(cur);
  int same = 1;

  if (type == VECSXP) {
    if (Rf_length(cur) != 2) {
      Rf_errorcall(R_NilValue,
                   _("when repeating matrices you need to use 'list(mat, n)'"));
    }
    same = lotri::getSingleInt(VECTOR_ELT(cur, 1), NA_INTEGER);
    if (same == NA_INTEGER) {
      Rf_errorcall(
          R_NilValue,
          _("you can only repeat a matrix a single positive number of times"));
    }
    if (same <= 0) {
      Rf_errorcall(R_NilValue,
                   _("you need to repeat a matrix a positive number of times"));
    }
    cur = VECTOR_ELT(cur, 0);
    type = TYPEOF(cur);
  }

  // When named == 2, we need to check if this specific matrix has names
  // If it doesn't, we should set named = 0 to disable naming for all
  if (*named == 2) {
    // First try with requireNamed = true
    auto info = lotri::checkSymmetricNamedMatrix(cur, true);
    if (info.isValid) {
      if (info.fixedCount > 0)
        *fixed = 1;
      *estimate += info.estimateCount;
      return info.nrows * same;
    }
    // Matrix doesn't have names, try without naming requirement
    info = lotri::checkSymmetricNamedMatrix(cur, false);
    if (info.isValid) {
      *named = 0; // Disable naming for all matrices
      if (info.fixedCount > 0)
        *fixed = 1;
      *estimate += info.estimateCount;
      return info.nrows * same;
    }
  } else {
    int ret = isSymNameMat(cur, *named, fixed, estimate);
    if (ret > 0) {
      return ret * same;
    }
  }

  if (*named) {
    Rf_errorcall(R_NilValue,
                 _("list element %d is not a symmetric named matrix"), i + 1);
  } else {
    Rf_errorcall(R_NilValue, _("list element %d is not a symmetric matrix"),
                 i + 1);
  }

  return 0;
}

// Set string element with optional formatting
static inline void setStrElt(SEXP retN, SEXP colnames, int curBand, int j,
                             const char *fmt, bool doFormat, int *cnt,
                             int nsame) {
  if (doFormat && nsame > 1) {
    char out[100];
    std::snprintf(out, sizeof(out), fmt, (*cnt)++);
    SET_STRING_ELT(retN, curBand + j, Rf_mkChar(out));
  } else if (!Rf_isNull(colnames)) {
    SET_STRING_ELT(retN, curBand + j, STRING_ELT(colnames, j));
  }
  // If colnames is NULL and we're not formatting, leave the string element
  // as-is
}

// Get double value from named vector
static inline double getDouble(SEXP colnames, int i, SEXP inUpperLower,
                               SEXP upperLowerNames, double defaultValue,
                               int /* type */) {
  const char *lookup = CHAR(STRING_ELT(colnames, i));
  const int upperLowerNamesSize = Rf_length(upperLowerNames);
  const int inUpperLowerSize = Rf_length(inUpperLower);

  if (inUpperLowerSize != upperLowerNamesSize) {
    Rf_errorcall(R_NilValue, _("malformed upper/lower names; names length and "
                               "vector length are unequal"));
  }

  for (int j = upperLowerNamesSize - 1; j >= 0; --j) {
    const char *current = CHAR(STRING_ELT(upperLowerNames, j));
    if (std::strcmp(current, lookup) == 0) {
      return REAL(inUpperLower)[j];
    }
  }

  return defaultValue;
}

// Set upper or lower bounds from input
static inline int setUpperLower(SEXP inUpperLower, SEXP colnames,
                                double *outUpperLower, int i0,
                                double defaultValue, const char * /* what */,
                                int nsame) {
  SEXP upperLowerNames = Rf_getAttrib(inUpperLower, R_NamesSymbol);
  double value = defaultValue;
  const int ncol = Rf_length(colnames);

  if (Rf_isNull(upperLowerNames)) {
    if (Rf_length(inUpperLower) == 1) {
      const int typ = TYPEOF(inUpperLower);
      if (typ == REALSXP) {
        value = REAL(inUpperLower)[0];
      } else if (typ == INTSXP) {
        value = static_cast<double>(INTEGER(inUpperLower)[0]);
      }
    } else if (Rf_length(inUpperLower) != 0) {
      return 1;
    }

    std::fill_n(outUpperLower + i0, ncol * nsame, value);
  } else {
    const int typ = TYPEOF(inUpperLower);
    for (int i = ncol - 1; i >= 0; --i) {
      outUpperLower[i0 + i] = getDouble(colnames, i, inUpperLower,
                                        upperLowerNames, defaultValue, typ);
    }

    // Copy for repeated matrices
    for (int i = 1; i < nsame; ++i) {
      std::memcpy(&outUpperLower[i0 + i * ncol], &outUpperLower[i0],
                  ncol * sizeof(double));
    }
  }

  return 0;
}

#endif // LOTRI_MATLIST_H_
