#include "unnest.h"

#define REP(N, len, i, j, body) do {              \
	i = j = 0;                                    \
	for (; i < N; j = (++j < len) ? j : 0, ++i) { \
	  body                                        \
	}                                             \
  } while (0)

SEXP rep_vector(SEXP x, R_xlen_t N) {

  R_xlen_t i, j, n = XLENGTH(x);
  if (n == 0)
    Rf_error("[Bug] Cannot replicate empty vector");

  SEXP out = PROTECT(allocVector(TYPEOF(x), N));

  switch (TYPEOF(x)) {
   case LGLSXP:
     REP(N, n, i, j, LOGICAL(out)[i] = LOGICAL(x)[j];); break;
   case INTSXP:
     REP(N, n, i, j, INTEGER(out)[i] = INTEGER(x)[j];); break;
   case REALSXP:
     REP(N, n, i, j, REAL(out)[i] = REAL(x)[j];); break;
   case CPLXSXP:
     REP(N, n, i, j, COMPLEX(out)[i] = COMPLEX(x)[j];); break;
   case RAWSXP:
     REP(N, n, i, j, RAW(out)[i] = RAW(x)[j];); break;
   case STRSXP:
     REP(N, n, i, j, SET_STRING_ELT(out, i, STRING_ELT(x, j));); break;
   case VECSXP:
   case EXPRSXP:
     REP(N, n, i, j, SET_VECTOR_ELT(out, i, lazy_duplicate(VECTOR_ELT(x, j))););
     break;
   default:
     Rf_error("Cannot unnest lists with elements of type %s", Rf_type2char(TYPEOF(x)));
  }

  Rf_setAttrib(out, R_ClassSymbol, Rf_getAttrib(x, R_ClassSymbol));

  if (Rf_inherits(x, "factor")) {
    setAttrib(out, R_LevelsSymbol, Rf_getAttrib(x, R_LevelsSymbol));
  }

  UNPROTECT(1);
  return out;
}
