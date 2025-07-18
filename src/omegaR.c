#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>
#include <Rmath.h>
#include "omega.h"

SEXP _lotriOmega_getBuiltinSize(void) {
  SEXP ret = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(ret)[0] = _lotriOmega_matSize();
  UNPROTECT(1);
  return ret;
}
