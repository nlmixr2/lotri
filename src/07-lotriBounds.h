#ifndef LOTRI_BOUNDS_H_
#define LOTRI_BOUNDS_H_

#include <algorithm>
#include <cstring>
#include <string_view>

namespace lotri {

// Set upper/lower bounds from property
inline bool setBoundsFromProperty(SEXP inUpperLower, SEXP colnames,
                                  double *outUpperLower, int i0,
                                  double defaultValue, std::string_view what,
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
      return true; // Error
    }

    std::fill_n(outUpperLower + i0, ncol * nsame, value);
  } else {
    const int typ = TYPEOF(inUpperLower);
    const int nNames = Rf_length(upperLowerNames);

    for (int i = 0; i < ncol; ++i) {
      const char *lookup = CHAR(STRING_ELT(colnames, i));
      bool found = false;

      for (int j = 0; j < nNames; ++j) {
        if (std::strcmp(lookup, CHAR(STRING_ELT(upperLowerNames, j))) == 0) {
          outUpperLower[i0 + i] =
              (typ == REALSXP) ? REAL(inUpperLower)[j]
                               : static_cast<double>(INTEGER(inUpperLower)[j]);
          found = true;
          break;
        }
      }

      if (!found) {
        outUpperLower[i0 + i] = defaultValue;
      }
    }

    // Copy for repeated matrices
    for (int i = 1; i < nsame; ++i) {
      std::memcpy(&outUpperLower[i0 + i * ncol], &outUpperLower[i0],
                  ncol * sizeof(double));
    }
  }

  return false; // Success
}

// Set string element with optional formatting
inline void setFormattedString(SEXP retN, SEXP colnames, int curBand, int j,
                               const char *fmt, bool doFormat, int *cnt,
                               int nsame) {
  if (doFormat && nsame > 1) {
    char out[100];
    std::snprintf(out, sizeof(out), fmt, (*cnt)++);
    SET_STRING_ELT(retN, curBand + j, Rf_mkChar(out));
  } else {
    SET_STRING_ELT(retN, curBand + j, STRING_ELT(colnames, j));
  }
}

// Create unbounded bounds (default -Inf to +Inf)
inline cpp4r::writable::list createUnboundedBounds(SEXP lst) {
  cpp4r::strings names = cpp4r::as_cpp<cpp4r::strings>(lotriAllNames(lst));
  const R_xlen_t len = names.size();

  cpp4r::writable::doubles boundLower(len);
  cpp4r::writable::doubles boundUpper(len);

  for (R_xlen_t i = 0; i < len; ++i) {
    boundLower[i] = R_NegInf;
    boundUpper[i] = R_PosInf;
  }

  boundLower.attr("names") = names;
  boundUpper.attr("names") = names;

  cpp4r::writable::list ret(2);
  ret[0] = boundLower;
  ret[1] = boundUpper;

  cpp4r::writable::strings retNames(2);
  retNames[0] = "lower";
  retNames[1] = "upper";
  ret.attr("names") = retNames;

  return ret;
}

} // namespace lotri

// Legacy compatibility wrapper
static inline bool lotriBoundsSetUpperLower(SEXP inUpperLower, SEXP colnames,
                                            double *outUpperLower, int i0,
                                            double defaultValue,
                                            const char *what, int nsame) {
  return lotri::setBoundsFromProperty(inUpperLower, colnames, outUpperLower, i0,
                                      defaultValue, what, nsame);
}

static inline void lotriBoundsSetStrElt(SEXP retN, SEXP colnames, int curBand,
                                        int j, const char *fmt, bool doFormat,
                                        int *cnt, int nsame) {
  lotri::setFormattedString(retN, colnames, curBand, j, fmt, doFormat, cnt,
                            nsame);
}

static inline cpp4r::writable::list lotriAssumeUnbounded(SEXP lst) {
  return lotri::createUnboundedBounds(lst);
}

/* roxygen
@title Get Bounds from lotri Object
@description Extract lower and upper bounds from a lotri matrix object.
@param lst A lotri list object
@param format Optional format string for naming
@param startNum Starting number for formatted names
@return A list with 'lower' and 'upper' named numeric vectors
@keywords internal
*/
[[cpp4r::register]] SEXP lotriGetBounds(SEXP lst, SEXP format, SEXP startNum) {
  if (TYPEOF(lst) != VECSXP) {
    cpp4r::stop("expects lotri matrix");
  }

  // If no lotri properties, assume unbounded
  if (Rf_isNull(Rf_getAttrib(lst, Rf_install("lotri")))) {
    return lotri::createUnboundedBounds(lst);
  }

  SEXP lotriProp = Rf_getAttrib(lst, Rf_install("lotri"));
  SEXP lotriPropNames = Rf_getAttrib(lotriProp, R_NamesSymbol);
  SEXP names = Rf_getAttrib(lst, R_NamesSymbol);

  lotriInfo li = lotriLstToMat0(lst, format, startNum);

  if (li.err == 1) {
    cpp4r::stop("'format' must be a single length string or NULL");
  }
  if (li.err == 2) {
    cpp4r::stop(
        "when format is specified, 'startNum' must be a single integer");
  }

  const int len = Rf_length(li.lst);
  int totdim = 0;
  int named = 1;

  for (int i = 0; i < len; ++i) {
    totdim += getCheckDim(li.lst, i, &named, &li.fix, &li.est);
  }

  cpp4r::writable::strings retN(totdim);
  cpp4r::writable::doubles boundLower(totdim);
  cpp4r::writable::doubles boundUpper(totdim);

  double *boundLowerD = REAL(boundLower);
  double *boundUpperD = REAL(boundUpper);

  int curBand = 0;
  bool badUpper = false;
  bool badLower = false;

  for (int i = 0; i < len && !badUpper && !badLower; ++i) {
    SEXP cur = VECTOR_ELT(li.lst, i);
    int type = TYPEOF(cur);
    int nsame = 1;

    if (type == VECSXP) {
      SEXP sameS = VECTOR_ELT(cur, 1);
      nsame = lotri::getSingleInt(sameS, 1);
      cur = VECTOR_ELT(cur, 0);
      type = TYPEOF(cur);
    }

    const int totN = Rf_ncols(cur);
    SEXP dimnames = Rf_getAttrib(cur, R_DimNamesSymbol);
    SEXP colnames = VECTOR_ELT(dimnames, 1);

    SEXP upper = getLotriProp(names, i, lotriProp, lotriPropNames, "upper");
    if (lotri::setBoundsFromProperty(upper, colnames, boundUpperD, curBand,
                                     R_PosInf, "upper", nsame)) {
      badUpper = true;
      break;
    }

    SEXP lower = getLotriProp(names, i, lotriProp, lotriPropNames, "lower");
    if (lotri::setBoundsFromProperty(lower, colnames, boundLowerD, curBand,
                                     R_NegInf, "lower", nsame)) {
      badLower = true;
      break;
    }

    for (int cursame = nsame; cursame-- > 0;) {
      for (int j = 0; j < totN; ++j) {
        lotri::setFormattedString(retN, colnames, curBand, j, li.fmt,
                                  li.doFormat != 0, &li.counter, nsame);
      }
      curBand += totN;
    }
  }

  if (badUpper) {
    cpp4r::stop("cannot figure out valid 'upper' properties");
  }
  if (badLower) {
    cpp4r::stop("cannot figure out valid 'lower' properties");
  }

  boundLower.attr("names") = retN;
  boundUpper.attr("names") = retN;

  cpp4r::writable::list ret(2);
  ret[0] = boundLower;
  ret[1] = boundUpper;

  cpp4r::writable::strings retNames(2);
  retNames[0] = "lower";
  retNames[1] = "upper";
  ret.attr("names") = retNames;

  return ret;
}

#endif // LOTRI_BOUNDS_H_
