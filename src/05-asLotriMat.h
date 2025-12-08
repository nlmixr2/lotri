#ifndef LOTRI_ASLOTRIMAT_H_
#define LOTRI_ASLOTRIMAT_H_

#include <cstring>
#include <string_view>

namespace lotri {

// Expand bounds with default values for all dimensions
inline cpp4r::writable::doubles expandBoundsWithDefault(SEXP cur,
                                                        cpp4r::strings dimn,
                                                        double defaultVal,
                                                        std::string_view what) {
  if (TYPEOF(cur) != REALSXP) {
    cpp4r::stop("'%s' needs to be a double", std::string(what).c_str());
  }

  const R_xlen_t nDim = dimn.size();
  cpp4r::writable::doubles ret(nDim);

  SEXP names = Rf_getAttrib(cur, R_NamesSymbol);

  if (Rf_isNull(names)) {
    if (Rf_xlength(cur) == 1) {
      // Single value: replicate for all dimensions
      const double val = REAL(cur)[0];
      for (R_xlen_t i = 0; i < nDim; ++i) {
        ret[i] = val;
      }
      ret.attr("names") = dimn;
      return ret;
    }
    cpp4r::stop("'%s' needs to be named", std::string(what).c_str());
  }

  // Named vector: match names to dimensions
  const R_xlen_t nNames = Rf_xlength(names);
  const double *inPtr = REAL(cur);

  for (R_xlen_t i = 0; i < nDim; ++i) {
    const char *targetName = CHAR(STRING_ELT(dimn, i));
    bool found = false;

    for (R_xlen_t j = 0; j < nNames; ++j) {
      if (std::strcmp(targetName, CHAR(STRING_ELT(names, j))) == 0) {
        ret[i] = inPtr[j];
        found = true;
        break;
      }
    }

    if (!found) {
      ret[i] = defaultVal;
    }
  }

  ret.attr("names") = dimn;
  return ret;
}

// Generate lotri matrix with properties
inline SEXP generateLotriMatrix(SEXP x, cpp4r::list extra, cpp4r::strings def,
                                cpp4r::strings dimn, std::string_view defVal) {
  // Create result list with the matrix
  cpp4r::writable::list ret(1);
  ret[0] = x;
  ret.attr("names") = def;

  // Create lotri class
  cpp4r::writable::strings lotriClass(1);
  lotriClass[0] = "lotri";

  const R_xlen_t lExtra = extra.size();

  if (lExtra == 0) {
    ret.attr("class") = lotriClass;
    return ret;
  }

  if (defVal.empty()) {
    cpp4r::stop(
        "extra properties need default try 'lotri(matrix,x=3,default=\"id\")'");
  }

  // Count non-null extras
  R_xlen_t nNull = 0;
  for (R_xlen_t i = 0; i < lExtra; ++i) {
    if (Rf_isNull(extra[i])) {
      ++nNull;
    }
  }

  cpp4r::strings extraNames =
      cpp4r::as_cpp<cpp4r::strings>(extra.attr("names"));

  // Create filtered extra list (without nulls)
  cpp4r::writable::list fextra(lExtra - nNull);
  cpp4r::writable::strings fextraN(lExtra - nNull);

  R_xlen_t j = 0;
  for (R_xlen_t i = lExtra; i-- > 0;) {
    if (Rf_isNull(extra[i]))
      continue;

    const char *curName = CHAR(STRING_ELT(extraNames, i));

    if (std::strcmp("lower", curName) == 0) {
      fextra[j] = expandBoundsWithDefault(extra[i], dimn, R_NegInf, "lower");
    } else if (std::strcmp("upper", curName) == 0) {
      fextra[j] = expandBoundsWithDefault(extra[i], dimn, R_PosInf, "upper");
    } else {
      fextra[j] = extra[i];
    }

    fextraN[j] = extraNames[i];
    ++j;
  }

  fextra.attr("names") = fextraN;

  // Create lotri property list
  cpp4r::writable::list lotriProp(1);
  lotriProp[0] = fextra;
  lotriProp.attr("names") = def;

  ret.attr("lotri") = lotriProp;
  ret.attr("class") = lotriClass;

  return ret;
}

} // namespace lotri

// Legacy compatibility wrapper
static inline cpp4r::writable::doubles asLotriMatAmpDefault(SEXP cur,
                                                            cpp4r::strings dimn,
                                                            double defaultVal,
                                                            const char *what) {
  return lotri::expandBoundsWithDefault(cur, dimn, defaultVal, what);
}

static inline SEXP asLotriMatGen(SEXP x, cpp4r::list extra, cpp4r::strings def,
                                 cpp4r::strings dimn, const char *defVal) {
  return lotri::generateLotriMatrix(x, extra, def, dimn, defVal);
}

/* roxygen
@title Convert Matrix to lotri Object
@description Convert a named symmetric matrix to a lotri object with optional
  bounds and other properties.
@param x A completely named symmetric matrix
@param extra A list of extra properties (lower, upper bounds, etc.)
@param def Default name for the matrix block (string of length 1)
@return A lotri object
@keywords internal
*/
[[cpp4r::register]] SEXP asLotriMat(SEXP x, cpp4r::list extra,
                                    cpp4r::strings def) {
  // Validate def parameter
  if (def.size() != 1) {
    cpp4r::stop("'default' must be a 'string' of length 1");
  }

  // Validate x is a matrix
  if (!Rf_isMatrix(x)) {
    cpp4r::stop("'x' needs to be a completely named matrix");
  }

  // Get and validate dimension names
  SEXP dims = Rf_getAttrib(x, R_DimNamesSymbol);
  if (Rf_isNull(dims)) {
    cpp4r::stop("'x' needs to be a completely named matrix");
  }

  SEXP dimn_sexp = VECTOR_ELT(dims, 0);
  if (Rf_isNull(dimn_sexp) || Rf_isNull(VECTOR_ELT(dims, 1))) {
    cpp4r::stop("'x' needs to be a completely named matrix");
  }

  cpp4r::strings dimn = cpp4r::as_cpp<cpp4r::strings>(dimn_sexp);
  const char *defVal = CHAR(STRING_ELT(def, 0));

  return lotri::generateLotriMatrix(x, extra, def, dimn, defVal);
}

#endif // LOTRI_ASLOTRIMAT_H_
