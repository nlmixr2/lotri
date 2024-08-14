#ifndef __NEARPD_H__
#define __NEARPD_H__
#if defined(__cplusplus)

using namespace arma;


bool lotriNearPDarma(mat &ret, mat x, bool keepDiag = true,
                  bool do2eigen = true, bool doDykstra = true, bool only_values = false,
                  double eig_tol   = 1e-6, double conv_tol  = 1e-7, double posd_tol  = 1e-8,
                  int maxit    = 1000, bool trace = false // set to TRUE (or 1 ..) to trace iterations
                  );

bool chol_sym(mat &Hout, mat& Hin);
bool inv_sym(mat &Hout, mat& Hin);

extern "C" {
#endif
  int lotriNearPDc(double *ret, double *x, int n, int keepDiag, int do2eigen, int doDykstra, int only_values,
                double eig_tol, double conv_tol, double posd_tol, int maxit, int trace);
#if defined(__cplusplus)
}
#endif
#endif
