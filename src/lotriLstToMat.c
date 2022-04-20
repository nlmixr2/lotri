#include "matlist.h"

SEXP lotriToLstMat(SEXP lotri) {
  SEXP lotriProp = Rf_getAttrib(lotri, Rf_install("lotri"));
  if (Rf_isNull(lotriProp)) {
    return lotri;
  }
  SEXP lotriNames = Rf_getAttrib(lotri, R_NamesSymbol);
  SEXP lotriPropNames = Rf_getAttrib(lotriProp, R_NamesSymbol);
  int pro=0;
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, Rf_length(lotri))); pro++;
  int nsame;
  for (int i = Rf_length(ret); i--;) {
    nsame = getSame(lotriNames, i, lotriProp, lotriPropNames);
    if (nsame > 1){
      SEXP cur = PROTECT(Rf_allocVector(VECSXP, 2)); pro++;
      SET_VECTOR_ELT(cur, 0, VECTOR_ELT(lotri, i));
      SEXP ns = PROTECT(Rf_allocVector(INTSXP, 1)); pro++;
      INTEGER(ns)[0] = nsame;
      SET_VECTOR_ELT(cur, 1, ns);
      SET_VECTOR_ELT(ret, i, cur);
    } else {
      SET_VECTOR_ELT(ret, i, VECTOR_ELT(lotri, i));
    }
  }
  UNPROTECT(pro);
  return ret;
}

lotriInfo assertCorrectMatrixProperties(SEXP lst_, SEXP format, SEXP startNum, int *named) {
  int type = TYPEOF(lst_);
  if (type != VECSXP) {
    int fixed = 0;
    int estimate = 0;
    if (isSymNameMat(lst_, *named, &fixed, &estimate)) {
      lotriInfo li;
      li.sym = 1;
      li.lst = R_NilValue;
      li.fix = fixed;
      li.est = estimate;
      return li;
    }
    Rf_errorcall(R_NilValue, _("expects a list named symmetric matrices"));
  }
  lotriInfo li = _lotriLstToMat0(lst_, format, startNum);
  PROTECT(li.lst);
  if (li.err == 1) {
    UNPROTECT(1);
    Rf_errorcall(R_NilValue, _("'format' must be a single length string or NULL"));
  }
  if (li.err == 2) {
    UNPROTECT(1);
    Rf_errorcall(R_NilValue, _("when format is specified, 'startNum' must be a single integer"));
  }
  UNPROTECT(1);
  return li;
}

SEXP _lotriEstDf(SEXP lst_, int totNum) {
  int i0 = 0, pro = 0;
  int lstLen = Rf_length(lst_);
  SEXP ret  = PROTECT(Rf_allocVector(VECSXP, 7)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 7)); pro++;

  SET_STRING_ELT(retN, 0, Rf_mkChar("name"));
  SEXP name = PROTECT(Rf_allocVector(STRSXP, totNum)); pro++;
  SET_VECTOR_ELT(ret, 0, name);

  SET_STRING_ELT(retN, 1, Rf_mkChar("lower"));
  SEXP lowerS = PROTECT(Rf_allocVector(REALSXP, totNum)); pro++;
  SET_VECTOR_ELT(ret, 1, lowerS);
  double *lower = REAL(lowerS);

  SET_STRING_ELT(retN, 2, Rf_mkChar("est"));
  SEXP estS = PROTECT(Rf_allocVector(REALSXP, totNum)); pro++;
  SET_VECTOR_ELT(ret, 2, estS);
  double *est = REAL(estS);

  SET_STRING_ELT(retN, 3, Rf_mkChar("upper"));
  SEXP upperS = PROTECT(Rf_allocVector(REALSXP, totNum)); pro++;
  SET_VECTOR_ELT(ret, 3, upperS);
  double *upper = REAL(upperS);

  SET_STRING_ELT(retN, 4, Rf_mkChar("fix"));
  SEXP fixS = PROTECT(Rf_allocVector(LGLSXP, totNum)); pro++;
  int *fix = INTEGER(fixS);
  SET_VECTOR_ELT(ret, 4, fixS);

  SET_STRING_ELT(retN, 5, Rf_mkChar("label"));
  SEXP label = PROTECT(Rf_allocVector(STRSXP, totNum)); pro++;
  SET_VECTOR_ELT(ret, 5, label);

  SET_STRING_ELT(retN, 6, Rf_mkChar("backTransform"));
  SEXP backTransform = PROTECT(Rf_allocVector(STRSXP, totNum)); pro++;
  SET_VECTOR_ELT(ret, 6, backTransform);

  for (int listCnt = 0; listCnt < lstLen; ++listCnt) {
    SEXP curV = Rf_getAttrib(VECTOR_ELT(lst_, listCnt), Rf_install("lotriEst"));
    if (!Rf_isNull(curV)) {
      SEXP nameIn = VECTOR_ELT(curV, 0);
      double *lowerIn = REAL(VECTOR_ELT(curV, 1));
      double *estIn = REAL(VECTOR_ELT(curV, 2));
      double *upperIn = REAL(VECTOR_ELT(curV, 3));
      int *fixIn = INTEGER(VECTOR_ELT(curV, 4));
      SEXP labelIn = VECTOR_ELT(curV, 5);
      SEXP backTransformIn = VECTOR_ELT(curV, 6);
      int inLen = Rf_length(nameIn);
      for (int inCnt = 0; inCnt < inLen; ++inCnt) {
	SET_STRING_ELT(name, i0, STRING_ELT(nameIn, inCnt));
	lower[i0] = lowerIn[inCnt];
	est[i0] = estIn[inCnt];
	upper[i0] = upperIn[inCnt];
	fix[i0] = fixIn[inCnt];
	SET_STRING_ELT(label, i0, STRING_ELT(labelIn, inCnt));
	SET_STRING_ELT(backTransform, i0, STRING_ELT(backTransformIn, inCnt));
	i0++;
      }
    }
  }

  SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(cls, 0, Rf_mkChar("data.frame"));
  Rf_classgets(ret, cls);

  SEXP rowNamesS = PROTECT(Rf_allocVector(INTSXP, 2)); pro++;
  int *rowNames = INTEGER(rowNamesS);
  rowNames[0] = NA_INTEGER;
  rowNames[1] = totNum;

  Rf_setAttrib(ret, R_NamesSymbol, retN);
  Rf_setAttrib(ret, Rf_install("row.names"), rowNamesS);

  UNPROTECT(pro);
  return ret;
}

SEXP _lotriLstToMat(SEXP lst_, SEXP format, SEXP startNum, SEXP matCls) {
  int pro = 0;
  int named = 2;
  lotriInfo li = assertCorrectMatrixProperties(lst_, format, startNum, &named);
  if (li.sym) return lst_;
  PROTECT(li.lst); pro++;
  int len = Rf_length(li.lst);
  int totdim = 0;
  int i;
  if (len == 2) {
    int repN = isSingleInt(VECTOR_ELT(li.lst, 1), NA_INTEGER);
    if (repN != NA_INTEGER && repN > 0) {
      if (isSymNameMat(VECTOR_ELT(li.lst, 0), named, &(li.fix), &(li.est))){
	SEXP new = PROTECT(Rf_allocVector(VECSXP, 1)); pro++;
	SET_VECTOR_ELT(new, 0, li.lst);
	SEXP ret = _lotriLstToMat(new, format, startNum, matCls);
	UNPROTECT(pro);
	return ret;
      }
    }
  }
  li.est = 0;
  for (i = 0; i < len; ++i) {
    totdim += getCheckDim(li.lst, i, &named, &(li.fix),  &(li.est));
  }
  int liEst = li.est;
  SEXP ret = PROTECT(Rf_allocMatrix(REALSXP, totdim, totdim)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, totdim)); pro++;
  double *retd = REAL(ret);
  int *retf = NULL;
  SEXP retF = R_NilValue;
  // Initialize to zero
  memset(retd, 0, sizeof(double)*totdim*totdim);
  if (li.fix) {
    retF = PROTECT(Rf_allocMatrix(LGLSXP, totdim, totdim)); pro++;
    retf = INTEGER(retF);
    // Set everything as FALSE
    memset(retf, 0, sizeof(int)*totdim*totdim);
  }
  // Now use memcpy/ integer conversion to c
  int curBand = 0;
  lotriLstToMatFillInFullMatrix(retd, retf, &totdim, retN, &curBand,
				&len, &li, &named);
  if (named) {
    SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, 2)); pro++;
    SET_VECTOR_ELT(dimnames, 0, retN);
    SET_VECTOR_ELT(dimnames, 1, retN);
    Rf_setAttrib(ret, R_DimNamesSymbol, dimnames);
    if (li.fix) Rf_setAttrib(retF, R_DimNamesSymbol, dimnames);
  }
  int doCls = 0;
  if (li.fix) {
    // Use the R 4.0 definition of matrix; It should work fine on R 3.0.x
    doCls = 1;
    Rf_setAttrib(ret, Rf_install("lotriFix"), retF);
  }
  if (liEst) {
    doCls = 1;
    SEXP liEstSEXP = PROTECT(_lotriEstDf(lst_, liEst)); pro++;
    Rf_setAttrib(ret, Rf_install("lotriEst"), liEstSEXP);
  }
  if (doCls) {
    int lenCls = Rf_length(matCls);
    SEXP cls = PROTECT(Rf_allocVector(STRSXP, lenCls+1)); pro++;
    SET_STRING_ELT(cls, 0, Rf_mkChar("lotriFix"));
    for (int mi = lenCls; mi--;) {
      SET_STRING_ELT(cls, mi+1, STRING_ELT(matCls, mi));
    }
    Rf_classgets(ret, cls);
  }
  UNPROTECT(pro);
  return ret;
}
