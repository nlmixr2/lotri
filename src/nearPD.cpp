// https://github.com/cran/Matrix/blob/master/R/nearPD.R
// Contributors Martin Maechler, Jens Oehlschlägel
#define ARMA_WARN_LEVEL 1
#include <cpp11armadillo.hpp>
#include "nearPD.h"

#include "cpp11/declarations.hpp"
//#include <R_ext/Visibility.h>


vec lotriRepEach(const vec& x, const int each) {
  std::size_t n=x.n_elem;
  std::size_t n_out=n*each;
  vec res(n_out);
  auto begin = res.begin();
  for (std::size_t i = 0, ind = 0; i < n; ind += each, ++i) {
    auto start = begin + ind;
    auto end = start + each;
    std::fill(start, end, x[i]);
  }
  return res;
}

mat lotriMatVecSameLen(mat mt1, vec v1){
  //do not check the input...
  int t=0;
  for(unsigned int i=0;i<mt1.n_cols;i++){
    for(unsigned int j=0;j<mt1.n_rows;j++){
      mt1(j,i)=mt1(j,i)*v1(t);
      t++;
    }
  }
  return(mt1);
}

vec lotriPmaxC(double a, vec b){
  vec c(b.n_elem);
  for(unsigned int i=0;i<b.n_elem;i++){
    c(i)=std::max(a,b(i));
  }
  return c;
}

bool eig_symR(vec &d, mat &Q, mat &B) {
  // This match's R style of eig_sym, to make translation easier
  mat B2 = 0.5*(B+B.t());
  if (!B2.is_symmetric()) return false;
  bool ret = eig_sym(d, Q, B2);
  if (!ret) return false;
  d = reverse(d);
  Q = fliplr(Q);
  return true;
}


bool lotriNearPDarma(mat &ret, mat x
                     , bool keepDiag// = false
                     , bool do2eigen// = true  // if TRUE do a sfsmisc::posdefify() eigen step
                     , bool doDykstra// = true // do use Dykstra's correction
                     , bool only_values// = false // if TRUE simply return lambda[j].
                     , double eig_tol//   = 1e-6 // defines relative positiveness of eigenvalues compared to largest
                     , double conv_tol//  = 1e-7 // convergence tolerance for algorithm
                     , double posd_tol//  = 1e-8 // tolerance for enforcing positive definiteness
                     , int maxit//    = 100 // maximum number of iterations allowed
                     , bool trace// = false // set to TRUE (or 1 ..) to trace iterations
                     ){
  unsigned int n = (unsigned int)x.n_cols;
  vec diagX0;
  if (keepDiag) {
    diagX0 = x.diag();
  }
  mat D_S(n, n, arma::fill::zeros);
  mat X = x;
  int iter = 0 ;
  bool converged = false;
  double conv = R_PosInf;
  mat Y;
  mat R;
  mat B;
  while (iter < maxit && !converged) {
    Y = X;
    if (doDykstra) {
      R = Y - D_S;
    }
    vec d;
    mat Qin;
    mat Q;
    if(doDykstra) {
      B=R;
    } else {
      B=Y;
    }
    if (!eig_symR(d, Q, B)) {
      return false;
    }

    // create mask from relative positive eigenvalues
    uvec p= (d>eig_tol*d[0]);
    if (sum(p)==0) {
      //stop("Matrix seems negative semi-definite")
      return false;
    }
    uvec fp = find(p);
    Q=Q.cols(fp);
    X=lotriMatVecSameLen(Q,lotriRepEach(d.elem(fp),Q.n_rows))*Q.t();
    // update Dykstra's correction D_S = \Delta S_k
    if (doDykstra) {
      D_S = X - R;
    }
    // project onto symmetric and possibly 'given diag' matrices:
    X = 0.5*(X + X.t());
    if (keepDiag) {
      X.diag() = diagX0;
    }
    conv = norm(Y-X,"inf")/norm(Y,"inf");
    iter = iter + 1;
    if (trace) {
      // cat(sprintf("iter %3d : #{p}=%d, ||Y-X|| / ||Y||= %11g\n",
      // iter, sum(p), conv))
      // Rcpp::Rcout << "iter " << iter <<" : #{p}= "<< sum(p) << std::endl;
      REprintf("iter %d: #{p}=%lld\n" , iter, sum(p));
    }
    converged = (conv <= conv_tol);
    // force symmetry is *NEVER* needed, we have symmetric X here!
    //X <- (X + t(X))/2
    if(do2eigen || only_values) {
      // begin from posdefify(sfsmisc)
      if (!eig_symR(d, Q, X)) {
        return false;
      }
      double Eps = posd_tol * std::abs(d[0]);
      if (d(n-1) < Eps) {
        d.elem(find(d < Eps)).fill(Eps);
        if (!only_values) {
          vec o_diag = X.diag();
          mat Q2 = Q.t();
          for (unsigned int i = 0; i < n; ++i)  {
            Q2.col(i) = d % Q2.col(i);
          }
          X = Q * Q2;
          vec D = sqrt(lotriPmaxC(Eps, o_diag)/X.diag());
          mat DX(n, n);
          mat D2(n, n);
          for (unsigned int i = 0; i < n; ++i)  {
            DX.col(i) = D % X.col(i);
            D2.col(i) = D;
          }
          X = DX % D2;
        }
        if (only_values) {
          ret = d;
          return true;
        }

        // unneeded(?!): X <- (X + t(X))/2
        if (keepDiag) {
          X.diag()= diagX0;
        }
      }
    } //end from posdefify(sfsmisc)
  }
  if(!converged){ //not converged
    return false;
  }
  ret = X;
  return true;
}

bool chol_sym(mat &Hout, mat &Hin) {
  mat H = 0.5*(Hin+Hin.t());
  if (!H.is_symmetric()) return false;
  return chol(Hout, H);
}

bool inv_sym(mat &Hout, mat &Hin) {
  mat H = 0.5*(Hin+Hin.t());
  if (!H.is_symmetric()) return false;
  return inv_sympd(Hout, H);
}

bool eig_sym2(vec &d, mat &Q, mat &B) {
  // This match's R style of eig_sym, to make translation easier
  mat B2 = 0.5*(B+B.t());
  if (!B2.is_symmetric()) return false;
  return eig_sym(d, Q, B2);
}

extern "C" int lotriNearPDc(double *ret, double *x, int n, int keepDiag,
                            int do2eigen, int doDykstra, int only_values,
                            double eig_tol, double conv_tol, double posd_tol, int maxit, int trace) {
  arma::mat xM(x, n, n, false, true);
  if (only_values) {
    arma::vec retV(ret, n, false, true);
     int res = lotriNearPDarma(retV, xM, keepDiag, do2eigen, doDykstra, only_values, eig_tol, conv_tol, posd_tol, maxit, trace);
    return res;
  } else {
    arma::mat retM(ret, n, n, false, true);
    int res = lotriNearPDarma(retM, xM, keepDiag, do2eigen, doDykstra, only_values, eig_tol, conv_tol, posd_tol, maxit, trace);
    return res;
  }
}
extern "C" SEXP _lotriNearPD_(SEXP xS
                             , SEXP keepDiagS
                             , SEXP do2eigenS  // if TRUE do a sfsmisc::posdefify() eigen step
                             , SEXP doDykstraS // do use Dykstra's correction
                             , SEXP only_valuesS // if TRUE simply return lambda[j].
                             , SEXP eig_tolS  // defines relative positiveness of eigenvalues compared to largest
                             , SEXP conv_tolS  // convergence tolerance for algorithm
                             , SEXP posd_tolS // tolerance for enforcing positive definiteness
                             , SEXP maxitS // maximum number of iterations allowed
                             , SEXP traceS // set to TRUE (or 1 ..) to trace iterations
                             ){
  int n = Rf_nrows(xS);
  int keepDiag = INTEGER(keepDiagS)[0];
  int do2eigen = INTEGER(do2eigenS)[0];
  int doDykstra = INTEGER(doDykstraS)[0];
  int only_values = INTEGER(only_valuesS)[0];
  double eig_tol = REAL(eig_tolS)[0];
  double conv_tol = REAL(conv_tolS)[0];
  double posd_tol = REAL(posd_tolS)[0];
  int maxit = INTEGER(maxitS)[0];
  int trace = INTEGER(traceS)[0];
  SEXP mat;
  if (only_values) {
    PROTECT(mat = Rf_allocVector(REALSXP, n));
  } else {
    PROTECT(mat = Rf_allocMatrix(REALSXP, n, n));
  }
  double *mat_ptr = REAL(mat);
  double *x_ptr = REAL(xS);
  try {
    if (lotriNearPDc(mat_ptr, x_ptr, n, keepDiag,
                     do2eigen, doDykstra, only_values,
                     eig_tol, conv_tol, posd_tol, maxit, trace)) {
      Rf_setAttrib(mat, R_DimNamesSymbol,  Rf_getAttrib(xS, R_DimNamesSymbol));
      UNPROTECT(1);
      return mat;
    } else {
      UNPROTECT(1);
      Rf_error("nearest PD calculation failed");
    }
  } catch (...) {
    UNPROTECT(1);
    Rf_error("unknown c++ error");
  }
  UNPROTECT(1);
  return R_NilValue;
}
