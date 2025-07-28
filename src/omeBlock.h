#ifndef __OME_BLOCK_H__
#define __OME_BLOCK_H__

#define omegaXformSqrt 1
#define omegaXformLog  2
#define omegaXformIdentity 3

#define omegaOpCholOmegaInv 0
#define omegaOpOmegaInv    -1
#define omegaOpNtheta      -2
#define omegaOpDOmegaInv   -3
#define omegaOpCholOmega1  -4
#define omegaOpOmega       -5
#define omegaOpCholOmega   -6
// log.det.OMGAinv.5
#define omegaOpLogDetOmegaInv -7
#define omegaOpOmega47        -8


#if defined(__cplusplus)
extern "C" {
#endif

  SEXP _lotri_omegaBlock(SEXP omeInS, SEXP diagXformS);
  SEXP _lotri_omegaBlockOp(SEXP omeBlock, SEXP opS);

#if defined(__cplusplus)
}
#endif


#endif // __OME_BLOCK_H__
