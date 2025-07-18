#ifndef __lotriOmegaArma_h__
#define __lotriOmegaArma_h__

extern "C" {
  typedef void (*_lotriOmega_mat_t)(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);
}

struct _lotriOmega_ind_omega {
  _lotriOmega_mat_t cFun = NULL;
  arma::vec theta;
  int dim;

  bool cholOmegaInvBool = false;
  arma::mat cholOmegaInvMat; // "chol.omegaInv"

  bool omegaInvBool = false;
  arma::mat omegaInvMat; // "omegaInv"

  bool dOmegaInvBool = false;
  arma::cube dOmegaInvCube; // dim x dim x ntheta "d.omegaInv"

  bool dDomegaInvBool = false;
  arma::mat dDomegaInvMat; // ntheta x dim; "d.D.omegaInv"

  bool cholOmega1Bool = false;
  arma::mat cholOmega1Mat; // "chol.omega1"

  bool omegaBool = false;
  arma::mat omegaMat; // "omega"

  bool cholOmegaBool = false;
  arma::mat cholOmegaMat; //"chol.omega"

  bool logDetOMGAinv5Bool = false;
  double logDetOMGAinv5double; //"log.det.OMGAinv.5"

  bool tr28Bool = false;
  arma::vec tr28vec; // "tr.28"

  // could be neta x neta x ntheta or dim x dim x ntheta
  bool omega47Bool = false;
  arma::cube omega47Cube;
};

class _lotriOmega_full_omega {
 public:
  _lotriOmega_full_omega() {

  }
  ~_lotriOmega_full_omega() {
    if (omes != NULL) free(omes);
  }
  _lotriOmega_ind_omega *omes = NULL;
  int nomes = 0;
  int nTotTheta = 0;
  int nTotDim = 0;
};

#define lotriOmega_sqrt 1
#define lotriOmega_log 2

#endif
