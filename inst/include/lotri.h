#ifndef LOTRI_H
#define LOTRI_H

#if defined(__cplusplus)
extern "C" {
#endif

  typedef SEXP (*lotriLstToMat_type)(SEXP, SEXP, SEXP, SEXP);
  extern lotriLstToMat_type lotriLstToMat;
  typedef SEXP (*lotriMat_type) (SEXP, SEXP, SEXP);
  extern lotriMat_type lotriMat;
  typedef SEXP (*asLotriMat_type) (SEXP, SEXP, SEXP);
  extern asLotriMat_type asLotriMat;
  typedef SEXP (*lotriSep_type) (SEXP, SEXP, SEXP, SEXP, SEXP);
  extern lotriSep_type lotriSep;
  typedef SEXP (*lotriAllNames_type) (SEXP);
  extern lotriAllNames_type lotriAllNames;
  typedef SEXP (*lotriGetBounds_type) (SEXP, SEXP, SEXP);
  extern lotriGetBounds_type lotriGetBounds;
  typedef SEXP (*isLotri_type) (SEXP);
  extern isLotri_type isLotri;
  typedef SEXP (*lotriMaxNu_type) (SEXP);
  extern lotriMaxNu_type lotriMaxNu;
  typedef SEXP (*lotriRcm_type) (SEXP);
  extern lotriRcm_type lotriRcm;

  typedef int (*lotriNearPDc_type)(double *, double *, int, int,
                                   int, int, int, double, double,
                                   double, int, int);
  extern lotriNearPDc_type lotriNearPDc;

  typedef SEXP (*lotriNearPDsexp_type) (SEXP, SEXP, SEXP, SEXP, SEXP,
                                        SEXP, SEXP, SEXP, SEXP, SEXP);
  extern lotriNearPDsexp_type lotriNearPDsexp;

  static inline void iniLotriPtr0(SEXP ptrLst) {
    //SEXP lotriLstToMatPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_lotriLstToMat, R_NilValue, R_NilValue)); pro++;
    if (lotriLstToMat == NULL) {
      lotriLstToMat = (lotriLstToMat_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 0));
      asLotriMat = (lotriMat_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 1));
      lotriSep = (lotriSep_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 2));
      lotriAllNames = (lotriAllNames_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 3));
      lotriGetBounds = (lotriGetBounds_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 4));
      lotriMaxNu = (lotriMaxNu_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 5));
      isLotri = (isLotri_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 6));
      lotriRcm = (lotriRcm_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 7));
      lotriNearPDc = (lotriNearPDc_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 8));
      lotriNearPDsexp = (lotriNearPDsexp_type) R_ExternalPtrAddrFn(VECTOR_ELT(ptrLst, 9));
    }
  }

#define iniLotri                                \
  asLotriMat_type asLotriMat;                   \
  lotriSep_type lotriSep;                       \
  lotriAllNames_type lotriAllNames;             \
  lotriGetBounds_type lotriGetBounds;           \
  lotriMaxNu_type lotriMaxNu;                   \
  isLotri_type isLotri;                         \
  lotriLstToMat_type lotriLstToMat;             \
  lotriRcm_type lotriRcm;                       \
  lotriNearPDc_type lotriNearPDc;               \
  lotriNearPDsexp_type lotriNearPDsexp;         \
  SEXP iniLotriPtr(SEXP ptr) {                  \
    iniLotriPtr0(ptr);                          \
    return R_NilValue;                          \
  }

#if defined(__cplusplus)
}

#define lotriNearPDarmaSetup                                            \
  static inline bool lotriNearPDarma(arma::mat &ret, arma::mat x, bool keepDiag = true, \
                                     bool do2eigen = true, bool doDykstra = true, bool only_values = false, \
                                     double eig_tol   = 1e-6, double conv_tol  = 1e-7, double posd_tol  = 1e-8, \
                                     int maxit    = 1000, bool trace = false \
                                     ) {                                \
    return lotriNearPDc(ret.memptr(), x.memptr(), x.n_rows,             \
                        keepDiag, do2eigen, doDykstra, only_values,     \
                        eig_tol, conv_tol, posd_tol, maxit, trace);     \
  }


#endif

#endif
