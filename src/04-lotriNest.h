#ifndef LOTRI_NEST_H_
#define LOTRI_NEST_H_

#include <cstring>

namespace lotri {

// Result structure for nested lotri operations
struct NestResult {
  SEXP ret{R_NilValue};
  int err{0};

  bool hasError() const noexcept { return err != 0; }
};

// Configuration for nested lotri extraction
struct NestConfig {
  int lenNest;
  int extra;
  int lotriLen;
  SEXP nestN;
  SEXP lotri;
  SEXP names;
  SEXP lotri0;
  SEXP lotri0names;
  SEXP sameC;
  int *nestI;
  SEXP nestStart;
};

// Expand nested lotri by adding ID if needed
inline SEXP expandNestById(const NestConfig &cfg, SEXP nestLotri,
                           SEXP nestLotriProp, NestResult &result) {
  if (cfg.extra == 0) {
    return cfg.nestN;
  }

  // Look for ID column
  bool found1 = false, found2 = false;
  SEXP nestN2 = PROTECT(Rf_allocVector(STRSXP, cfg.lenNest + cfg.extra));

  for (int j = 0; j < cfg.lotriLen && (!found1 || !found2); ++j) {
    if (!found1 && casecmp("id", CHAR(STRING_ELT(cfg.names, j))) == 0) {
      found1 = true;
      SET_STRING_ELT(nestN2, 0, STRING_ELT(cfg.names, j));
      SET_VECTOR_ELT(nestLotri, 0, VECTOR_ELT(cfg.lotri, j));
    }

    if (!found2 && casecmp("id", CHAR(STRING_ELT(cfg.lotri0names, j))) == 0) {
      SET_VECTOR_ELT(nestLotriProp, 0, VECTOR_ELT(cfg.lotri0, j));
      found2 = true;
    }
  }

  if (!found1 || !found2) {
    result.err = 2;
    UNPROTECT(1);
    return cfg.nestN;
  }

  // Copy remaining names
  for (int i = 0; i < cfg.lenNest; ++i) {
    SET_STRING_ELT(nestN2, i + 1, STRING_ELT(cfg.nestN, i));
  }

  UNPROTECT(1);
  return nestN2;
}

// Get nested lotri matrix with specified configuration
inline NestResult getNested(const NestConfig &cfg) {
  NestResult result;

  // Validate nestStart
  if (TYPEOF(cfg.nestStart) != INTSXP || Rf_length(cfg.nestStart) != 1) {
    result.err = 1;
    return result;
  }

  const int totalLen = cfg.lenNest + cfg.extra;

  SEXP nestLotri = PROTECT(Rf_allocVector(VECSXP, totalLen));
  SEXP nestLotriProp = PROTECT(Rf_allocVector(VECSXP, totalLen));
  SEXP lotriClass = PROTECT(Rf_allocVector(STRSXP, 1));

  SET_STRING_ELT(lotriClass, 0, Rf_mkChar("lotri"));
  Rf_setAttrib(nestLotri, Rf_install("lotri"), nestLotriProp);

  SEXP nestN2 = PROTECT(expandNestById(cfg, nestLotri, nestLotriProp, result));

  if (result.hasError()) {
    UNPROTECT(4);
    return result;
  }

  Rf_setAttrib(nestLotri, R_NamesSymbol, nestN2);
  Rf_setAttrib(nestLotriProp, R_NamesSymbol, nestN2);

  // Match and assign matrices
  for (int i = cfg.extra; i < cfg.lenNest + cfg.extra; ++i) {
    bool found1 = false, found2 = false;
    const char *curNest = CHAR(STRING_ELT(nestN2, i));

    for (int j = 0; j < cfg.lotriLen && (!found1 || !found2); ++j) {
      if (!found1 &&
          std::strcmp(curNest, CHAR(STRING_ELT(cfg.names, j))) == 0) {
        found1 = true;
        SET_VECTOR_ELT(nestLotri, i, VECTOR_ELT(cfg.lotri, j));
      }

      if (!found2 &&
          std::strcmp(curNest, CHAR(STRING_ELT(cfg.lotri0names, j))) == 0) {
        SET_VECTOR_ELT(
            nestLotriProp, i,
            addPropertyAtEnd(cfg.lotri0, i, cfg.sameC, cfg.nestI, cfg.extra));
        found2 = true;
      }
    }

    if (!found1 || !found2) {
      result.err = 3;
      UNPROTECT(4);
      return result;
    }
  }

  // Set format based on extra flag
  SEXP format = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(format, 0, Rf_mkChar(cfg.extra ? "ETA[%d]" : "THETA[%d]"));

  Rf_setAttrib(nestLotri, R_ClassSymbol, lotriClass);
  Rf_setAttrib(nestLotri, Rf_install("format"), format);
  Rf_setAttrib(nestLotri, Rf_install("start"), cfg.nestStart);

  result.ret = nestLotri;
  UNPROTECT(5);
  return result;
}

} // namespace lotri

// Legacy compatibility types and functions
typedef struct lotriNestInfo {
  SEXP ret;
  int err;
} lotriNestInfo;

typedef struct lotriNestGet {
  int lenNest;
  int extra;
  int lotriLen;
  SEXP nestN;
  SEXP lotri;
  SEXP names;
  SEXP lotri0;
  SEXP lotri0names;
  SEXP sameC;
  int *nestI;
  SEXP nestStart;
} lotriNestGet;

static inline lotriNestInfo getNestLotri(int lenNest, int extra, int lotriLen,
                                         SEXP nestN, SEXP lotri, SEXP names,
                                         SEXP lotri0, SEXP lotri0names,
                                         SEXP sameC, int *nestI,
                                         SEXP nestStart) {
  lotri::NestConfig cfg;
  cfg.lenNest = lenNest;
  cfg.extra = extra;
  cfg.lotriLen = lotriLen;
  cfg.nestN = nestN;
  cfg.lotri = lotri;
  cfg.names = names;
  cfg.lotri0 = lotri0;
  cfg.lotri0names = lotri0names;
  cfg.sameC = sameC;
  cfg.nestI = nestI;
  cfg.nestStart = nestStart;

  lotri::NestResult result = lotri::getNested(cfg);

  lotriNestInfo info;
  info.ret = result.ret;
  info.err = result.err;
  return info;
}

#endif // LOTRI_NEST_H_
