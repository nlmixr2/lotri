#include <armadillo.hpp>
#include <cpp11.hpp>
#include <cpp11armadillo.hpp>
#include "cpp11/declarations.hpp"
// Function to perform the RCM algorithm
extern "C" SEXP _lotri_rcm_(SEXP As) {
  int pro = 0;
  try {
    cpp11::doubles_matrix<> Asd = as_cpp<cpp11::doubles_matrix<>>(As);
    mat A = as_Mat(Asd);
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
    SEXP dimN2 = PROTECT(Rf_allocVector(STRSXP, n)); pro++;
    SEXP dimN = PROTECT(VECTOR_ELT(Rf_getAttrib(As, R_DimNamesSymbol), 0)); pro++;
    for (uword i = 0; i < n; i++) {
      SET_STRING_ELT(dimN2, i, STRING_ELT(dimN, perm(i)));
    }
    mat A2 = A(perm, perm);
    cpp11::writable::list dn(2);
    dn[0] = dimN2;
    dn[1] = dimN2;
    SEXP ret = PROTECT(as_sexp(A2)); pro++;
    cpp11::writable::integers dim(2);
    dim[0] = n;
    dim[1] = n;
    Rf_setAttrib(ret, R_DimSymbol, as_sexp(dim));
    Rf_setAttrib(ret, R_DimNamesSymbol, dn);
    UNPROTECT(pro);
    return ret;
  } catch (...) {
    UNPROTECT(pro);
    Rf_error("Error in RCM algorithm");
  }
  return R_NilValue;
}
