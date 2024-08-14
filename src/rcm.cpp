#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// Function to perform the RCM algorithm
extern "C" SEXP _lotri_rcm_(SEXP As) {
  CharacterVector dimN = VECTOR_ELT(Rf_getAttrib(As, R_DimNamesSymbol), 0);
  mat A = as<arma::mat>(As);
  uword n = A.n_rows;
  uvec nonZero(n);
  uvec perm(n);
  // Fill the permutations to values outside the range of the matrix
  std::fill_n(perm.begin(), n, n+1);
  uword minNonZero = n+1;
  // Store the number of non-zero elements in each row
  // also find the row with the fewest non-zero elements
  // which is the first row in the permutation
  for (uword i = 0; i < n; i++) {
    nonZero(i) = sum(A.row(i) != 0);
    if (nonZero(i) < minNonZero) {
      perm(0) = i;
      minNonZero = nonZero(i);
    }
  }
  nonZero(perm(0)) = n+2; // Remove the first row from the list of remaining rows

  for (uword i = 0; i < n-1; i++) {
    // Which row elements do not contain zero
    uvec nonZeroRows = find(A.row(perm(i)) != 0);
    // Take out the rows that have already been permuted
    nonZeroRows = nonZeroRows(find(nonZero(nonZeroRows) < n+2));
    if (nonZeroRows.n_elem == 0) {
      // Here we find the minimum number of non-zero elements in the remaining rows and
      // add it to the row permutation
      minNonZero = n+1;
      for (uword j = 0; j < n; j++) {
        if (nonZero(j) < minNonZero) {
          perm(i+1) = j;
          minNonZero = nonZero(j);
        }
      }
      nonZero(perm(i+1)) = n+2; // Remove the row from the list of remaining rows
    } else {
      // Sort nonZeroRows in decending order by the number of non-zero elements
      uvec sortedNonZeroRows = sort_index(nonZero(nonZeroRows), "descend");
      // now add the sortedZero rows to the permutation
      for (uword j = 0; j < sortedNonZeroRows.n_elem; j++) {
        perm(i+j+1) = nonZeroRows(sortedNonZeroRows(j));
        nonZero(perm(i+j+1)) = n+2; // Remove the row from the list of remaining rows
      }
      // skip the rows that have been added to the permutation
      i += sortedNonZeroRows.n_elem - 1;
    }
  }
  perm = flipud(perm);
  CharacterVector dimN2(n);
  for (uword i = 0; i < n; i++) {
    dimN2(i) = dimN(perm(i));
  }
  mat A2 = A(perm, perm);
  RObject ret = wrap(A2);
  ret.attr("dimnames") = List::create(dimN2, dimN2);
  // Reverse the permutation
  return wrap(ret);
}
