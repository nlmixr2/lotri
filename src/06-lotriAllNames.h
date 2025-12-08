// ============================================================================
// lotriAllNames - Get all parameter names from lotri object
// ============================================================================

#ifndef LOTRI_ALL_NAMES_H
#define LOTRI_ALL_NAMES_H

namespace lotri {
namespace names {

// Get dimension names from a matrix safely
inline SEXP getMatrixDimNames(SEXP mat) {
  if (!Rf_isMatrix(mat)) {
    return R_NilValue;
  }
  return Rf_getAttrib(mat, R_DimNamesSymbol);
}

// Get column names from dimension names
inline SEXP getColNames(SEXP dimnames) {
  if (Rf_isNull(dimnames)) {
    return R_NilValue;
  }
  SEXP colnames = VECTOR_ELT(dimnames, 1);
  if (Rf_isNull(colnames)) {
    return VECTOR_ELT(dimnames, 0); // Fall back to row names
  }
  return colnames;
}

} // namespace names
} // namespace lotri

/* roxygen
@title Get All Names from lotri Object
@description Extract all parameter names from a lotri matrix object.
@param lotri A lotri object (matrix or list of matrices)
@return Character vector of all parameter names
@keywords internal
*/
[[cpp4r::register]] cpp4r::writable::strings lotriAllNames(SEXP lotri) {
  using namespace lotri::names;

  // Handle matrix case
  if (Rf_isMatrix(lotri)) {
    SEXP dimnames = getMatrixDimNames(lotri);
    if (Rf_isNull(dimnames)) {
      return cpp4r::writable::strings(static_cast<R_xlen_t>(0));
    }

    SEXP colnames = getColNames(dimnames);
    if (Rf_isNull(colnames)) {
      return cpp4r::writable::strings(static_cast<R_xlen_t>(0));
    }

    return cpp4r::as_cpp<cpp4r::writable::strings>(colnames);
  }

  // Handle list case (list of matrices)
  if (TYPEOF(lotri) == VECSXP) {
    R_xlen_t listLen = Rf_xlength(lotri);

    // Count total names
    int totalNames = 0;
    for (R_xlen_t i = 0; i < listLen; ++i) {
      SEXP dimnames = Rf_getAttrib(VECTOR_ELT(lotri, i), R_DimNamesSymbol);
      if (!Rf_isNull(dimnames)) {
        SEXP colnames = VECTOR_ELT(dimnames, 1);
        if (!Rf_isNull(colnames)) {
          totalNames += Rf_length(colnames);
        }
      }
    }

    // Collect names in reverse order (matching original behavior)
    cpp4r::writable::strings ret(totalNames);
    int j = 0;
    for (R_xlen_t i = listLen; i-- > 0;) {
      SEXP dimnames = Rf_getAttrib(VECTOR_ELT(lotri, i), R_DimNamesSymbol);
      if (Rf_isNull(dimnames))
        continue;

      SEXP colnames = VECTOR_ELT(dimnames, 1);
      if (Rf_isNull(colnames))
        continue;

      R_xlen_t nCols = Rf_xlength(colnames);
      for (R_xlen_t k = 0; k < nCols; ++k) {
        ret[j++] = STRING_ELT(colnames, k);
      }
    }

    return ret;
  }

  cpp4r::stop("not a matrix or lotri matrix");
  return cpp4r::writable::strings(static_cast<R_xlen_t>(0)); // Never reached
}

#endif // LOTRI_ALL_NAMES_H
