#ifndef OMEGA_FROM_R_H
#define OMEGA_FROM_R_H

#if defined(__cplusplus)
extern "C" {
#endif

  SEXP _lotri_lotriOmegaNew(SEXP omeListS, SEXP diagXformS);
  SEXP _lotri_getTheta(SEXP inSEXP);
  SEXP _lotri_setTheta(SEXP inSEXP, SEXP thetaS);
  SEXP _lotri_getCholOmegaInv(SEXP inSEXP);
  SEXP _lotri_getOmegaInv(SEXP inSEXP);
  SEXP _lotri_getdDomegaInv(SEXP inSEXP);
  SEXP _lotri_getCholOmega1(SEXP inSEXP);
  SEXP _lotri_getOmegaR(SEXP inSEXP);
  SEXP _lotri_getCholOmega(SEXP inSEXP);
  SEXP _lotri_getLogDetOMGAinv5(SEXP inSEXP);
  SEXP _lotri_tr28(SEXP inSEXP);

#if defined(__cplusplus)
}
#endif
#endif // OMEGA_FROM_R_H
