#define ARMA_WARN_LEVEL 1

#include <armadillo4r.hpp>
#include <cpp4r.hpp>

#include "01-matlist.h"
#include "02-lotriProp.h"
#include "03-lotriLstToMat.h"
#include "04-lotriNest.h"
#include "05-asLotriMat.h"
#include "06-lotriAllNames.h"
#include "07-lotriBounds.h"
#include "08-nearPD.h"
#include "09-rcm.h"

// ============================================================================
// Namespace for main.cpp helper functions
// ============================================================================

namespace lotri {
namespace main {

// Error codes for lotriSep validation
enum class SepError {
  None = 0,
  InvalidStart = 1,
  IdNotFound = 2,
  NamesMismatch = 3
};

// Validate named integer vector input
inline void validateNamedIntegerVector(SEXP vec, const char *name) {
  SEXP names = Rf_getAttrib(vec, R_NamesSymbol);
  if (Rf_isNull(names)) {
    cpp4r::stop("'%s' needs to be named", name);
  }
  if (TYPEOF(vec) != INTSXP) {
    cpp4r::stop("'%s' needs to be an integer", name);
  }
}

// Handle error from getNestLotri
inline void handleNestError(int err, bool isAbove) {
  const char *prefix = isAbove ? "above" : "below";
  switch (err) {
  case 1:
    cpp4r::stop("'%sStart' needs to be an 'integer' of length 1", prefix);
    break;
  case 2:
    cpp4r::stop("'id' not found in 'lotri' matrix");
    break;
  case 3:
    cpp4r::stop("'%s' names do not match 'lotri' matrix", prefix);
    break;
  }
}

} // namespace main
} // namespace lotri

// ============================================================================
// Exported functions that depend on the headers above
// ============================================================================

/* roxygen
@title Separate lotri Matrix into Above and Below
@description Separate a lotri matrix into components above and below certain
thresholds.
@param x A lotri matrix object
@param above Named integer vector for 'above' components
@param below Named integer vector for 'below' components
@param aboveStart Starting number for above formatting
@param belowStart Starting number for below formatting
@return A list with 'above' and 'below' components
@keywords internal
*/
[[cpp4r::register]] cpp4r::writable::list
lotriSep(cpp4r::list lotri, SEXP above, cpp4r::integers below, SEXP aboveStart,
         SEXP belowStart) {
  using namespace lotri::main;

  // Get lotri names and properties
  cpp4r::strings names = lotri.names();
  SEXP lotri_sexp = cpp4r::as_sexp(lotri);
  SEXP lotri0 = Rf_getAttrib(lotri_sexp, Rf_install("lotri"));

  if (Rf_isNull(lotri0)) {
    lotri0 = blankProp(cpp4r::as_sexp(names));
  }

  SEXP lotri0names = Rf_getAttrib(lotri0, R_NamesSymbol);
  R_xlen_t lotriLen = names.size();

  if (lotriLen != Rf_xlength(lotri0)) {
    cpp4r::stop("'lotri' malformed");
  }

  // Validate below input
  validateNamedIntegerVector(cpp4r::as_sexp(below), "below");

  R_xlen_t lenAbove = Rf_isNull(above) ? 0 : Rf_xlength(above);
  R_xlen_t lenBelow = below.size();

  // Create result list
  cpp4r::writable::list ret(2);
  cpp4r::writable::strings retN(2);
  retN[0] = "above";
  retN[1] = "below";
  ret.attr("names") = retN;

  SEXP sameC = Rf_mkChar("same");

  // Process above component
  if (lenAbove == 0) {
    ret[0] = R_NilValue;
  } else {
    validateNamedIntegerVector(above, "above");

    SEXP aboveN = Rf_getAttrib(above, R_NamesSymbol);
    int *aboveI = INTEGER(above);

    lotriNestInfo curLT =
        getNestLotri(static_cast<int>(lenAbove), 0, static_cast<int>(lotriLen),
                     aboveN, lotri_sexp, cpp4r::as_sexp(names), lotri0,
                     lotri0names, sameC, aboveI, aboveStart);

    if (curLT.err != 0) {
      handleNestError(curLT.err, true);
    }
    ret[0] = curLT.ret;
  }

  // Process below component
  SEXP belowN = Rf_getAttrib(cpp4r::as_sexp(below), R_NamesSymbol);
  int *belowI = INTEGER(cpp4r::as_sexp(below));

  lotriNestInfo curLT2 =
      getNestLotri(static_cast<int>(lenBelow), 1, static_cast<int>(lotriLen),
                   belowN, lotri_sexp, cpp4r::as_sexp(names), lotri0,
                   lotri0names, sameC, belowI, belowStart);

  if (curLT2.err != 0) {
    handleNestError(curLT2.err, false);
  }
  ret[1] = curLT2.ret;

  return ret;
}

/* roxygen
@title Check if Object is a lotri Object
@description Check if an R object is a valid lotri object.
@param lotri An R object to check
@return Logical indicating if the object is a lotri object
@keywords internal
*/
[[cpp4r::register]] bool isLotri(SEXP lotri) {
  // Check for lotri attribute first
  SEXP lotriProp = Rf_getAttrib(lotri, Rf_install("lotri"));
  if (!Rf_isNull(lotriProp)) {
    return true;
  }

  // Must be a list of named matrices
  if (TYPEOF(lotri) != VECSXP) {
    return false;
  }

  R_xlen_t n = Rf_xlength(lotri);
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP elem = VECTOR_ELT(lotri, i);

    if (!Rf_isMatrix(elem)) {
      return false;
    }

    SEXP dimnames = Rf_getAttrib(elem, R_DimNamesSymbol);
    if (Rf_isNull(dimnames)) {
      return false;
    }
  }

  return true;
}

// ============================================================================
// External pointer functions for C-callable interface
// ============================================================================

namespace {

// No-op finalizer for external pointers (functions don't need cleanup)
void lotriFunNoFree(SEXP) {}

// Helper to create and register an external function pointer
SEXP makeExternalFnPtr(DL_FUNC func) {
  SEXP ptr = R_MakeExternalPtrFn(func, R_NilValue, R_NilValue);
  R_RegisterCFinalizer(ptr, lotriFunNoFree);
  return ptr;
}

} // anonymous namespace

/* roxygen
@title Get lotri C Function Pointers
@description Get external pointers to lotri's C functions for use by other
packages.
@return A named list of external pointers to C functions
@keywords internal
*/
[[cpp4r::register]] cpp4r::writable::list getLotriPointers() {
  // Function names for the exported pointers
  constexpr int numFunctions = 9;
  const char *functionNames[numFunctions] = {
      "lotriLstToMat", "asLotriMat",     "lotriSep",
      "lotriAllNames", "lotriGetBounds", "lotriMaxNu",
      "isLotri",       "lotriRcm",       "lotriNearPD"};

  // Create external pointers for each function
  cpp4r::writable::list ret(numFunctions);
  ret[0] = makeExternalFnPtr((DL_FUNC)&lotriLstToMat);
  ret[1] = makeExternalFnPtr((DL_FUNC)&asLotriMat);
  ret[2] = makeExternalFnPtr((DL_FUNC)&lotriSep);
  ret[3] = makeExternalFnPtr((DL_FUNC)&lotriAllNames);
  ret[4] = makeExternalFnPtr((DL_FUNC)&lotriGetBounds);
  ret[5] = makeExternalFnPtr((DL_FUNC)&lotriMaxNu);
  ret[6] = makeExternalFnPtr((DL_FUNC)&isLotri);
  ret[7] = makeExternalFnPtr((DL_FUNC)&rcm);
  ret[8] = makeExternalFnPtr((DL_FUNC)&nearPD_);

  // Set names
  cpp4r::writable::strings retN(numFunctions);
  for (int i = 0; i < numFunctions; ++i) {
    retN[i] = functionNames[i];
  }
  ret.attr("names") = retN;

  return ret;
}
