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

  extern int _lotriLoaded;

  static inline void iniLotriPtr0(SEXP ptrLst) {
    //SEXP lotriLstToMatPtr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)&_lotriLstToMat, R_NilValue, R_NilValue)); pro++;
    if (_lotriLoaded == 0) {
      lotriLstToMat = (lotriLstToMat_type) R_ExternalPtrAddr(VECTOR_ELT(ptrLst, 0));
      asLotriMat = (lotriMat_type) R_ExternalPtrAddr(VECTOR_ELT(ptrLst, 1));
      lotriSep = (lotriSep_type) R_ExternalPtrAddr(VECTOR_ELT(ptrLst, 2));
      lotriAllNames = (lotriAllNames_type) R_ExternalPtrAddr(VECTOR_ELT(ptrLst, 3));
      lotriGetBounds = (lotriGetBounds_type) R_ExternalPtrAddr(VECTOR_ELT(ptrLst, 4));
      lotriMaxNu = (lotriMaxNu_type) R_ExternalPtrAddr(VECTOR_ELT(ptrLst, 5));
      isLotri = (isLotri_type) R_ExternalPtrAddr(VECTOR_ELT(ptrLst, 6));
      _lotriLoaded = 1;
    }
  }

  #define iniLotri \
    asLotriMat_type asLotriMat; \
    lotriSep_type lotriSep; \
    lotriAllNames_type lotriAllNames; \
    lotriGetBounds_type lotriGetBounds; \
    lotriMaxNu_type lotriMaxNu; \
    isLotri_type isLotri; \
    lotriLstToMat_type lotriLstToMat;
    int _lotriLoaded = 0; \
    SEXP iniLotriPtr(SEXP ptr) {                     \
      iniLotriPtr0(ptr);                             \
      return R_NilValue;                             \
    }

#if defined(__cplusplus)
}
#endif

#endif
