#include <deque>
#include <cstring>
#include "unnest.h"


#define REP(from, to, len, i, j, body) do {        \
	i = from;                                      \
    j = 0;                                         \
	for (; i < to; j = (++j < len) ? j : 0, ++i) { \
	  body                                         \
	}                                              \
  } while (0)


#define MemNA(p,n,na)  memset(p, na, (R_SIZE_T)(n) * sizeof(*p))

void fill_vector(SEXP source, SEXP target, R_xlen_t from, R_xlen_t to) {

  R_xlen_t i, j, n = XLENGTH(source);
  if (n == 0)
    Rf_error("[Bug] Cannot replicate empty vector");

  if (TYPEOF(source) != TYPEOF(target))
    Rf_error("[Bug] Type of source must be the same as of the target");

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
     MemNA(INTEGER(out), len, NA_INTEGER); break;
   case REALSXP:
     MemNA(REAL(out), len, NA_REAL); break;
   case CPLXSXP:
     MemNA(COMPLEX(out), len, NA_REAL); break;
   case RAWSXP:
     // for RAW we set 0s
     MemNA(RAW(out), len, 0); break;
   case STRSXP:
     SEXP* data = STRING_PTR(out);
     for (R_xlen_t i = 0; i < len; i++)
       data[i] = R_NaString;
  }
  // list/expression are already initialized to "NULL" by allocVector
  return out;
}
