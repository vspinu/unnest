
#include "common.h"
#include "Spec.h"

Spec list2spec(SEXP lspec) {
  if (TYPEOF(lspec) != VECSXP)
    Rf_error("'spec' must be a list");
  if (!isSpec(lspec))
    Rf_error("'spec' must be of S3 class 'unnest.spec'");

  SEXP names = Rf_getAttrib(lspec, R_NamesSymbol);
  if (names == R_NilValue)
    Rf_error("unnest.spec must have non-nil names");

  R_xlen_t N = LENGTH(lspec);
  bool
    done_node = false, done_as = false,
    done_children = false, done_stack = false,
    done_exclude = false;
  SEXP children = R_NilValue;

  Spec spec;

  for (R_xlen_t i = 0; i < N; i++) {
    SEXP obj = VECTOR_ELT(lspec, i);
    if (obj != R_NilValue) {
      const char* nm = CHAR(STRING_ELT(names, i));

      if (!done_node && !strcmp(nm, "node")) {
        if (XLENGTH(obj) != 1)
          Rf_error("spec's 'node' fields must be of length 1");
        switch(TYPEOF(obj)) {
         case STRSXP: spec.node = STRING_ELT(obj, 0); break;
         case INTSXP: spec.ix = INTEGER(obj)[0] - 1; break;
         case REALSXP: spec.ix = REAL(obj)[0] - 1; break;
         default:
           Rf_error("spec's 'node' field must be NULL, string or numeric vector");
        }
        done_node = true;
      } else  if (!done_as && !strcmp(nm, "as")) {
        if (TYPEOF(obj) != STRSXP || XLENGTH(obj) != 1)
          Rf_error("spec's 'as' field must be a string vector of length 1");
        spec.name = STRING_ELT(obj, 0);
        done_as = true;
      } else if (!done_stack && !strcmp(nm, "stack")) {
        if (TYPEOF(obj) != LGLSXP || XLENGTH(obj) != 1)
          Rf_error("spec's 'stack' field must be a logical vector of length 1");
        spec.stack = LOGICAL(obj)[0];
        done_stack = true;
      } else if (!done_children && !strcmp(nm, "children")) {
        if (TYPEOF(obj) != VECSXP)
          Rf_error("spec's 'children' field must be a list");
        children = obj;
        done_children = true;
      } else if (!done_exclude && !strcmp(nm, "exclude")) {
        if (TYPEOF(obj) != STRSXP)
          Rf_error("spec's 'exclude' field must be a character vector");
        spec.exclude = obj;
        done_exclude = true;
      }

    }
  }

  // always has name unless node is NULL
  if (spec.node != R_NilValue && spec.name == R_NilValue)
    spec.name = spec.node;

  if (children != R_NilValue) {
    R_xlen_t NC = XLENGTH(children);
    spec.children.reserve(NC);
    for (R_xlen_t c = 0; c < NC; c++) {
      SEXP ch = VECTOR_ELT(children, c);
      spec.children.emplace_back(list2spec(ch));
    }
  }
  return spec;
}

bool isSpec(SEXP s) {
   SEXP cls;
   if (OBJECT(s)) {
     cls = getAttrib(s, R_ClassSymbol);
     for (int i = 0; i < LENGTH(cls); i++)
       if (!strcmp(CHAR(STRING_ELT(cls, i)), "unnest.spec"))
         return true;
   }
   return false;
 }
