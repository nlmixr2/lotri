#include <RcppArmadillo/Lighter>
// std::fill_n is used directly below; see the note in nearPD.cpp.
#include <algorithm>
// Function to perform the RCM algorithm
extern "C" SEXP _lotri_rcm_(SEXP As) {
  BEGIN_RCPP
    arma::mat A = Rcpp::as<arma::mat>(As);
    arma::uword n = A.n_rows;
    // Guard: n=0 causes n-1 to wrap to UWORD_MAX (unsigned underflow) in the
    // loop below, producing an effectively infinite loop.
    if (n == 0) {
      Rcpp::Shield<SEXP> ret(Rf_allocMatrix(REALSXP, 0, 0));
      Rf_setAttrib(ret, R_DimNamesSymbol, Rf_getAttrib(As, R_DimNamesSymbol));
      return ret;
    }
    arma::uvec nonZero(n);
    arma::uvec perm(n);
    // Fill the permutations to values outside the range of the matrix
    std::fill_n(perm.begin(), n, n+1);
    arma::uword minNonZero = n+1;
    // Store the number of non-zero elements in each row
    // also find the row with the fewest non-zero elements
    // which is the first row in the permutation
    for (arma::uword i = 0; i < n; i++) {
      nonZero(i) = arma::sum(A.row(i) != 0);
      if (nonZero(i) < minNonZero) {
        perm(0) = i;
        minNonZero = nonZero(i);
      }
    }
    nonZero(perm(0)) = n+2; // Remove the first row from the list of remaining rows

    for (arma::uword i = 0; i < n-1; i++) {
      // Which row elements do not contain zero
      arma::uvec nonZeroRows = arma::find(A.row(perm(i)) != 0);
      // Take out the rows that have already been permuted
      nonZeroRows = nonZeroRows(arma::find(nonZero(nonZeroRows) < n+2));
      if (nonZeroRows.n_elem == 0) {
        // Here we find the minimum number of non-zero elements in the remaining rows and
        // add it to the row permutation
        minNonZero = n+1;
        for (arma::uword j = 0; j < n; j++) {
          if (nonZero(j) < minNonZero) {
            perm(i+1) = j;
            minNonZero = nonZero(j);
          }
        }
        nonZero(perm(i+1)) = n+2; // Remove the row from the list of remaining rows
      } else {
        // Sort nonZeroRows in decending order by the number of non-zero elements
        arma::uvec sortedNonZeroRows = arma::sort_index(nonZero(nonZeroRows), "descend");
        // now add the sortedZero rows to the permutation
        for (arma::uword j = 0; j < sortedNonZeroRows.n_elem; j++) {
          perm(i+j+1) = nonZeroRows(sortedNonZeroRows(j));
          nonZero(perm(i+j+1)) = n+2; // Remove the row from the list of remaining rows
        }
        // skip the rows that have been added to the permutation
        i += sortedNonZeroRows.n_elem - 1;
      }
    }
    perm = arma::flipud(perm);

    Rcpp::Shield<SEXP> dimN2(Rf_allocVector(STRSXP, n));
    SEXP asDimnames = Rf_getAttrib(As, R_DimNamesSymbol);
    SEXP dimN = VECTOR_ELT(asDimnames, 0);
    for (arma::uword i = 0; i < n; i++) {
      SET_STRING_ELT(dimN2, i, STRING_ELT(dimN, perm(i)));
    }

    arma::mat A2 = A(perm, perm);
    Rcpp::Shield<SEXP> ret(Rcpp::wrap(A2));
    Rf_setAttrib(ret, R_DimNamesSymbol, Rcpp::List::create((SEXP)dimN2, (SEXP)dimN2));
    return ret;
  END_RCPP
}
