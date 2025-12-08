#ifndef LOTRI_NEARPD_H_
#define LOTRI_NEARPD_H_

#include <algorithm>
#include <cmath>

// nearPD - Nearest Positive Definite Matrix
// Based on: https://github.com/cran/Matrix/blob/master/R/nearPD.R
// Contributors: Martin Maechler, Jens Oehlschlägel

namespace lotri {

// Repeat each element of vector x 'each' times
inline arma::vec repeatEach(const arma::vec &x, arma::uword each) {
  const arma::uword n = x.n_elem;
  arma::vec res(n * each);

  for (arma::uword i = 0; i < n; ++i) {
    res.subvec(i * each, (i + 1) * each - 1).fill(x[i]);
  }

  return res;
}

// Element-wise multiply matrix columns by vector (column-major order)
inline arma::mat multiplyMatrixByVector(arma::mat m, const arma::vec &v) {
  arma::uword idx = 0;
  for (arma::uword j = 0; j < m.n_cols; ++j) {
    for (arma::uword i = 0; i < m.n_rows; ++i) {
      m(i, j) *= v(idx++);
    }
  }
  return m;
}

// Parallel maximum: max(a, b[i]) for each element
inline arma::vec parallelMax(double a, const arma::vec &b) {
  return arma::max(arma::vec(b.n_elem, arma::fill::value(a)), b);
}

// Eigenvalue decomposition matching R's style (descending order)
inline bool eigenSymmetricR(arma::vec &d, arma::mat &Q, const arma::mat &B) {
  arma::mat B2 = 0.5 * (B + B.t());
  if (!B2.is_symmetric())
    return false;

  if (!arma::eig_sym(d, Q, B2))
    return false;

  // R returns eigenvalues in descending order
  d = arma::reverse(d);
  Q = arma::fliplr(Q);
  return true;
}

// Configuration for nearPD algorithm
struct NearPDConfig {
  bool keepDiag{false};
  bool do2eigen{true};
  bool doDykstra{true};
  bool onlyValues{false};
  double eigTol{1e-06};
  double convTol{1e-07};
  double posdTol{1e-08};
  int maxIter{100};
  bool trace{false};
};

// Core nearPD algorithm using Armadillo
inline bool computeNearestPD(arma::mat &ret, arma::mat x,
                             const NearPDConfig &cfg) {
  const arma::uword n = x.n_cols;
  arma::vec diagX0;

  if (cfg.keepDiag) {
    diagX0 = x.diag();
  }

  arma::mat D_S(n, n, arma::fill::zeros);
  arma::mat X = x;
  int iter = 0;
  bool converged = false;
  double conv = R_PosInf;

  while (iter < cfg.maxIter && !converged) {
    arma::mat Y = X;
    arma::mat B = cfg.doDykstra ? (Y - D_S) : Y;

    arma::vec d;
    arma::mat Q;
    if (!eigenSymmetricR(d, Q, B)) {
      return false;
    }

    // Create mask from relative positive eigenvalues
    arma::uvec p = (d > cfg.eigTol * d[0]);
    if (arma::sum(p) == 0) {
      return false; // Matrix seems negative semi-definite
    }

    arma::uvec fp = arma::find(p);
    Q = Q.cols(fp);
    X = multiplyMatrixByVector(Q, repeatEach(d.elem(fp), Q.n_rows)) * Q.t();

    // Update Dykstra's correction
    if (cfg.doDykstra) {
      D_S = X - B;
    }

    // Project onto symmetric matrices
    X = 0.5 * (X + X.t());

    if (cfg.keepDiag) {
      X.diag() = diagX0;
    }

    conv = arma::norm(Y - X, "inf") / arma::norm(Y, "inf");
    ++iter;

    if (cfg.trace) {
      REprintf("iter %d: #{p}=%llu\n", iter,
               static_cast<unsigned long long>(arma::sum(p)));
    }

    converged = (conv <= cfg.convTol);

    if (cfg.do2eigen || cfg.onlyValues) {
      if (!eigenSymmetricR(d, Q, X)) {
        return false;
      }

      const double Eps = cfg.posdTol * std::abs(d[0]);

      if (d(n - 1) < Eps) {
        d.elem(arma::find(d < Eps)).fill(Eps);

        if (!cfg.onlyValues) {
          arma::vec o_diag = X.diag();
          arma::mat Q2 = Q.t();

          for (arma::uword i = 0; i < n; ++i) {
            Q2.col(i) = d % Q2.col(i);
          }

          X = Q * Q2;
          arma::vec D = arma::sqrt(parallelMax(Eps, o_diag) / X.diag());

          // Apply diagonal scaling
          X.each_col() %= D;
          X.each_row() %= D.t();
        }

        if (cfg.onlyValues) {
          ret = d;
          return true;
        }

        if (cfg.keepDiag) {
          X.diag() = diagX0;
        }
      }
    }
  }

  if (!converged) {
    return false;
  }

  ret = X;
  return true;
}

// Symmetric Cholesky decomposition
inline bool choleskySymmetric(arma::mat &Hout, const arma::mat &Hin) {
  arma::mat H = 0.5 * (Hin + Hin.t());
  if (!H.is_symmetric())
    return false;
  return arma::chol(Hout, H);
}

// Symmetric matrix inverse
inline bool inverseSymmetric(arma::mat &Hout, const arma::mat &Hin) {
  arma::mat H = 0.5 * (Hin + Hin.t());
  if (!H.is_symmetric())
    return false;
  return arma::inv_sympd(Hout, H);
}

} // namespace lotri

// Legacy compatibility wrappers
static inline arma::vec lotriRepEach(const arma::vec &x, arma::uword each) {
  return lotri::repeatEach(x, each);
}

static inline arma::mat lotriMatVecSameLen(arma::mat m, const arma::vec &v) {
  return lotri::multiplyMatrixByVector(std::move(m), v);
}

static inline arma::vec lotriPmaxC(double a, const arma::vec &b) {
  return lotri::parallelMax(a, b);
}

static inline bool eig_symR(arma::vec &d, arma::mat &Q, const arma::mat &B) {
  return lotri::eigenSymmetricR(d, Q, B);
}

static inline bool lotriNearPDarma(arma::mat &ret, arma::mat x, bool keepDiag,
                                   bool do2eigen, bool doDykstra,
                                   bool only_values, double eig_tol,
                                   double conv_tol, double posd_tol, int maxit,
                                   bool trace) {
  lotri::NearPDConfig cfg;
  cfg.keepDiag = keepDiag;
  cfg.do2eigen = do2eigen;
  cfg.doDykstra = doDykstra;
  cfg.onlyValues = only_values;
  cfg.eigTol = eig_tol;
  cfg.convTol = conv_tol;
  cfg.posdTol = posd_tol;
  cfg.maxIter = maxit;
  cfg.trace = trace;
  return lotri::computeNearestPD(ret, std::move(x), cfg);
}

static inline bool chol_sym(arma::mat &Hout, const arma::mat &Hin) {
  return lotri::choleskySymmetric(Hout, Hin);
}

static inline bool inv_sym(arma::mat &Hout, const arma::mat &Hin) {
  return lotri::inverseSymmetric(Hout, Hin);
}

/* roxygen
@title Nearest Positive Definite Matrix
@description Compute the nearest positive definite matrix to an approximate one,
  typically a correlation or variance-covariance matrix.
@param x_r A symmetric matrix
@param keepDiag Logical indicating if the diagonal should be preserved
@param do2eigen Logical indicating if eigenvalue step should be performed
@param doDykstra Logical indicating if Dykstra's correction should be used
@param only_values Logical indicating if only eigenvalues should be returned
@param eig_tol Tolerance for eigenvalue positiveness
@param conv_tol Convergence tolerance for the algorithm
@param posd_tol Tolerance for enforcing positive definiteness
@param maxit Maximum number of iterations
@param trace Logical indicating if iterations should be traced
@return The nearest positive definite matrix, or eigenvalues if only_values is
TRUE
@keywords internal
*/
[[cpp4r::register]] SEXP nearPD_(cpp4r::doubles_matrix<> x_r, bool keepDiag,
                                 bool do2eigen, bool doDykstra,
                                 bool only_values, double eig_tol,
                                 double conv_tol, double posd_tol, int maxit,
                                 bool trace) {
  arma::mat x = as_mat(x_r);

  lotri::NearPDConfig cfg;
  cfg.keepDiag = keepDiag;
  cfg.do2eigen = do2eigen;
  cfg.doDykstra = doDykstra;
  cfg.onlyValues = only_values;
  cfg.eigTol = eig_tol;
  cfg.convTol = conv_tol;
  cfg.posdTol = posd_tol;
  cfg.maxIter = maxit;
  cfg.trace = trace;

  if (only_values) {
    arma::mat result;
    if (!lotri::computeNearestPD(result, std::move(x), cfg)) {
      cpp4r::stop("nearest PD calculation failed");
    }
    return as_sexp(result);
  }

  arma::mat result;
  if (!lotri::computeNearestPD(result, std::move(x), cfg)) {
    cpp4r::stop("nearest PD calculation failed");
  }

  cpp4r::writable::doubles_matrix<> ret = as_doubles_matrix(result);
  ret.attr("dimnames") = x_r.attr("dimnames");

  return ret;
}

#endif // LOTRI_NEARPD_H_
