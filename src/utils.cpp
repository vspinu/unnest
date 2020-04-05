#include <deque>
#include <cstring>
#include <algorithm>
#include "common.h"


#define REP(from, to, len, i, j, body) do {        \
	i = from;                                      \
    j = 0;                                         \
	for (; i < to; j = (++j < len) ? j : 0, ++i) { \
	  body                                         \
	}                                              \
  } while (0)

#define REP1(from, to, i, body) for (i = from; i < to;  ++i) { body }


#define FillNA(p,n,na)  std::fill(p, p + n, na)

void fill_vector(SEXP source, SEXP target, R_xlen_t from, R_xlen_t to) {

  R_xlen_t i, j, n = XLENGTH(source);
  if (n == 0)
    Rf_error("[Bug] Cannot replicate empty vector");

  if (TYPEOF(source) != TYPEOF(target))
    Rf_error("[Bug] Type of source (%s) must be the same as that of the target (%s)",
             Rf_type2char(TYPEOF(source)), Rf_type2char(TYPEOF(target)));

  switch (TYPEOF(target)) {
   case LGLSXP:
     REP(from, to, n, i, j, LOGICAL(target)[i] = LOGICAL(source)[j];); break;
   case INTSXP:
     REP(from, to, n, i, j, INTEGER(target)[i] = INTEGER(source)[j];); break;
   case REALSXP:
     REP(from, to, n, i, j, REAL(target)[i] = REAL(source)[j];); break;
   case CPLXSXP:
     REP(from, to, n, i, j, COMPLEX(target)[i] = COMPLEX(source)[j];); break;
   case RAWSXP:
     REP(from, to, n, i, j, RAW(target)[i] = RAW(source)[j];); break;
   case STRSXP:
     REP(from, to, n, i, j, SET_STRING_ELT(target, i, STRING_ELT(source, j));); break;
   case VECSXP:
   case EXPRSXP:
     REP(from, to, n, i, j, SET_VECTOR_ELT(target, i, lazy_duplicate(VECTOR_ELT(source, j))););
     break;
   default:
     Rf_error("Cannot unnest lists with elements of type %s", Rf_type2char(TYPEOF(source)));
  }

  // FIXME: Add factor levels

}

void fill_vector_1(SEXP source, R_xlen_t source_ix, SEXP target, R_xlen_t from, R_xlen_t to) {

  if (TYPEOF(source) != TYPEOF(target))
    Rf_error("[Bug] Type of source (%s) must be the same as that of the target (%s)",
             Rf_type2char(TYPEOF(source)), Rf_type2char(TYPEOF(target)));

  R_xlen_t i;
  switch (TYPEOF(target)) {
   case LGLSXP: {
     int val = LOGICAL(source)[source_ix];
     REP1(from, to, i, LOGICAL(target)[i] = val;);
   }; break;
   case INTSXP: {
     int val = INTEGER(source)[source_ix];
     REP1(from, to, i, INTEGER(target)[i] = val;);
   }; break;
   case REALSXP: {
     double val = REAL(source)[source_ix];
     REP1(from, to, i, REAL(target)[i] = val;);
   }; break;
   case CPLXSXP: {
     Rcomplex val = COMPLEX(source)[source_ix];
     REP1(from, to, i, COMPLEX(target)[i] = val;);
   }; break;
   case RAWSXP: {
     Rbyte val = RAW(source)[source_ix];
     REP1(from, to, i, RAW(target)[i] = val;);
   }; break;
   case STRSXP: {
     SEXP val = STRING_ELT(source, source_ix);
     REP1(from, to, i, SET_STRING_ELT(target, i, val););
   }; break;
   case VECSXP:
   case EXPRSXP: {
     SEXP val = VECTOR_ELT(source, source_ix);
     REP1(from, to, i, SET_VECTOR_ELT(target, i, lazy_duplicate(val)););
   }; break;
   default:
     Rf_error("Cannot unnest lists with elements of type %s", Rf_type2char(TYPEOF(source)));
  }

  // FIXME: Add factor levels

}

SEXP extract_scalar(SEXP x, R_xlen_t ix) {

  switch (TYPEOF(x)) {
   case LGLSXP:
     return Rf_ScalarLogical(LOGICAL(x)[ix]);
   case INTSXP:
     return Rf_ScalarInteger(INTEGER(x)[ix]);
   case REALSXP:
     return Rf_ScalarReal(REAL(x)[ix]);
   case CPLXSXP:
     return Rf_ScalarComplex(COMPLEX(x)[ix]);
   case RAWSXP:
     return Rf_ScalarRaw(RAW(x)[ix]);
   case STRSXP:
     return Rf_ScalarString(STRING_ELT(x, ix));
   default:
     Rf_error("Cannot extract scalar from a vector of type %s", Rf_type2char(TYPEOF(x)));
  }

}

SEXP rep_vector(SEXP x, R_xlen_t N) {

  R_xlen_t i, j, n = XLENGTH(x);
  if (n == 0)
    Rf_error("[Bug] Cannot replicate empty vector");

  SEXP out = PROTECT(allocVector(TYPEOF(x), N));

  fill_vector(x, out, 0, N);

  Rf_setAttrib(out, R_ClassSymbol, Rf_getAttrib(x, R_ClassSymbol));

  // fixme: speed this up with the logic akin to that in isFrame
  if (Rf_inherits(x, "factor")) {
    setAttrib(out, R_LevelsSymbol, Rf_getAttrib(x, R_LevelsSymbol));
  }

  UNPROTECT(1);
  return out;
}



SEXP make_na_vector(SEXPTYPE type, R_xlen_t len) {
  SEXP out;

  switch (type) {
   case LGLSXP:
   case INTSXP:
   case REALSXP:
   case CPLXSXP:
   case STRSXP:
   case EXPRSXP:
   case VECSXP:
   case RAWSXP:
     out = allocVector(type, len); break;
   default:
     error("Cannot make a vector of mode '%s'", Rf_type2char(type));
  }

  switch (type) {
   case INTSXP:
   case LGLSXP:
     FillNA(INTEGER(out), len, NA_INTEGER); break;
   case REALSXP:
     FillNA(REAL(out), len, NA_REAL); break;
   case STRSXP:
     {
       SEXP* sdata = STRING_PTR(out);
       for (R_xlen_t i = 0; i < len; i++) {
         sdata[i] = R_NaString;
       }
     }
     break;
   case CPLXSXP:
     {
       Rcomplex* cdata = COMPLEX(out);
       for (R_xlen_t i = 0; i < len; i++) {
         cdata[i].r = NA_REAL;
         cdata[i].i = NA_REAL;
       }
     }
     break;
   case RAWSXP:
     // for RAW we set 0s
     FillNA(RAW(out), len, 0);
  }
  // list/expression are already initialized to "NULL" by allocVector
  return out;
}
