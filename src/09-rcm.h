#ifndef LOTRI_RCM_H_
#define LOTRI_RCM_H_

#include <algorithm>
#include <string>

// ============================================================================
// RCM Algorithm Implementation
// ============================================================================

namespace lotri {
namespace rcm {

// Validate matrix input for RCM algorithm
inline void validateRcmInput(const arma::mat &A, SEXP x_sexp) {
  // Validate: must be square
  if (A.n_rows != A.n_cols) {
    cpp4r::stop("'x' must be a square matrix");
  }

  // Validate: no NA/NaN values
  if (A.has_nan() || A.has_inf()) {
    cpp4r::stop("'x' must not contain missing or infinite values");
  }

  // Validate dimension names
  SEXP dimnames = Rf_getAttrib(x_sexp, R_DimNamesSymbol);
  if (Rf_isNull(dimnames)) {
    cpp4r::stop("'x' must have dimension names");
  }

  SEXP rownames = VECTOR_ELT(dimnames, 0);
  SEXP colnames = VECTOR_ELT(dimnames, 1);

  if (Rf_isNull(rownames) || Rf_isNull(colnames)) {
    cpp4r::stop("'x' must have both row and column names");
  }

  // Check that row names and column names are identical
  R_xlen_t n_names = Rf_xlength(rownames);
  for (R_xlen_t i = 0; i < n_names; ++i) {
    const char *rn = CHAR(STRING_ELT(rownames, i));
    const char *cn = CHAR(STRING_ELT(colnames, i));
    if (std::strcmp(rn, cn) != 0) {
      cpp4r::stop("The matrix must have matching row and column names");
    }
  }
}

// Compute RCM permutation order
inline arma::uvec computeRcmPermutation(const arma::mat &A) {
  const arma::uword n = A.n_rows;
  const arma::uword sentinel = n + 1;
  const arma::uword processed = n + 2;

  arma::uvec nonZeroCounts(n);
  arma::uvec perm(n, arma::fill::value(sentinel));

  // Count non-zero elements per row and find starting row
  arma::uword minCount = sentinel;
  for (arma::uword i = 0; i < n; ++i) {
    nonZeroCounts(i) = arma::sum(A.row(i) != 0);
    if (nonZeroCounts(i) < minCount) {
      perm(0) = i;
      minCount = nonZeroCounts(i);
    }
  }

  nonZeroCounts(perm(0)) = processed; // Mark as processed

  // Build permutation using RCM algorithm
  for (arma::uword i = 0; i < n - 1; ++i) {
    // Find non-zero neighbors of current row
    arma::uvec neighbors = arma::find(A.row(perm(i)) != 0);

    // Filter out already processed rows
    neighbors = neighbors(arma::find(nonZeroCounts(neighbors) < processed));

    if (neighbors.is_empty()) {
      // Find unprocessed row with minimum non-zero count
      minCount = sentinel;
      for (arma::uword j = 0; j < n; ++j) {
        if (nonZeroCounts(j) < minCount) {
          perm(i + 1) = j;
          minCount = nonZeroCounts(j);
        }
      }
      nonZeroCounts(perm(i + 1)) = processed;
    } else {
      // Sort neighbors by non-zero count (descending) and add to permutation
      arma::uvec sortOrder =
          arma::sort_index(nonZeroCounts(neighbors), "descend");

      for (arma::uword j = 0; j < sortOrder.n_elem; ++j) {
        perm(i + j + 1) = neighbors(sortOrder(j));
        nonZeroCounts(perm(i + j + 1)) = processed;
      }

      i += sortOrder.n_elem - 1; // Skip processed entries
    }
  }

  return arma::flipud(perm); // Reverse for RCM
}

} // namespace rcm
} // namespace lotri

/* roxygen
@title Use the RCM algorithm to permute to banded matrix
@description The RCM stands for the reverse Cuthill McKee (RCM) algorithm
  which is used to permute the matrix to a banded matrix.
@param x A symmetric matrix
@return A permuted matrix that should be banded
@export
@examples
m <- lotri({
 a + b + c + d + e + f + g + h + i + j + k + l + m + n + o +
 p ~ c(0.4, 0, 0.3, 0, 0, 0, -0.1, 0, 0, 0.2, 0, 0, 0,
       0, 0.5, 0, 0, 0, 0, 0, 1.3, 0, 0, 0, 0, 0, -0.6, 0.8,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.2, 0, 0.3,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.1, 0.2, 0, 0, 0.2,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, -1.1,
       0.9, 0, 0, 0, 0, 0, 0, 0, 4.7, 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0.5, 0, 0.2, 0, 0, 0, 1.9)
})

rcm(m)
*/
[[cpp4r::register]] cpp4r::doubles_matrix<> rcm(cpp4r::doubles_matrix<> x) {
  using namespace lotri::rcm;

  arma::mat A = as_mat(x);
  SEXP x_sexp = as_sexp(x);

  validateRcmInput(A, x_sexp);

  const arma::uword n = A.n_rows;
  arma::uvec perm = computeRcmPermutation(A);

  // Get original dimension names
  cpp4r::strings dimN = cpp4r::as_cpp<cpp4r::strings>(
      VECTOR_ELT(Rf_getAttrib(x_sexp, R_DimNamesSymbol), 0));

  // Create new dimension names based on permutation
  cpp4r::writable::strings dimN2(n);
  for (arma::uword i = 0; i < n; ++i) {
    dimN2[static_cast<R_xlen_t>(i)] = dimN[static_cast<R_xlen_t>(perm(i))];
  }

  // Apply permutation to matrix
  arma::mat permutedA = A(perm, perm);

  // Convert to R matrix and set attributes
  cpp4r::writable::doubles_matrix<> ret = as_doubles_matrix(permutedA);

  cpp4r::writable::list dn(2);
  dn[0] = dimN2;
  dn[1] = dimN2;
  ret.attr("dimnames") = dn;

  return ret;
}

#endif // LOTRI_RCM_H_
