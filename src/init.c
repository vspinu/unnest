#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP C_unnest(SEXP x, SEXP lspec, SEXP stack);

static const R_CallMethodDef CallEntries[] = {
  {"C_unnest", (DL_FUNC) &C_unnest, 3},
  {NULL, NULL, 0}
};

void R_init_unnest(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
