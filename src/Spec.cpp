
#include "common.h"
#include "Spec.h"

vector<SpecMatch> Spec::match(SEXP obj) const {
  int N = LENGTH(obj);
  SEXP obj_names = Rf_getAttrib(obj, R_NamesSymbol);
  bool has_names = obj_names != R_NilValue;

  vector<SpecMatch> out;

  if (ix >= 0) {
    // 1) ix has the highest priority
    if (ix < N) {
      SEXP nm = has_names ? STRING_ELT(obj_names, ix) : R_NilValue;
      out.emplace_back(ix, name, nm, VECTOR_ELT(obj, ix));
    }
  } else if (node == R_NilValue) {
    // 2) NULL node matches all
    if (include == R_NilValue) {
      out.reserve(N);
    } else {
      if (!has_names)
        return (out);
    }
    for (int i = 0; i < N; i++) {
      SEXP nm = R_NilValue;
      if (has_names) {
        nm = STRING_ELT(obj_names, i);
        if (is_char_in_strvec(nm, exclude)) {
          continue;
        } else if (include != R_NilValue) {
          if (!is_char_in_strvec(nm, include)) {
            continue;
          }
        }
      }
      out.emplace_back(i, nm, nm, VECTOR_ELT(obj, i));
    }
  } else if (has_names) {
    // 3) Exact node match
    for (size_t i = 0; i < N; i++) {
      if (STRING_ELT(obj_names, i) == node) {
        out.emplace_back(i, name, node, VECTOR_ELT(obj, i));
        break;
      }
    }
  }

  return out;
}

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
    done_children = false, done_groups = false,
    done_stack = false, done_include = false,
    done_exclude = false, done_dedupe = false;
  SEXP children = R_NilValue, groups = R_NilValue;

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
        if (!(TYPEOF(obj) == LGLSXP || TYPEOF(obj) == STRSXP) || XLENGTH(obj) != 1)
          Rf_error("spec's 'stack' field must be a logical or a character vector of length 1");
        if (TYPEOF(obj) == LGLSXP) {
          spec.stack = LOGICAL(obj)[0];
        } else {
          spec.stack = true;
          spec.ix_name = STRING_ELT(obj, 0);
        }
        done_stack = true;
      } else if (!done_children && !strcmp(nm, "children")) {
        if (TYPEOF(obj) != VECSXP)
          Rf_error("spec's 'children' field must be a list");
        children = obj;
        done_children = true;
      } else if (!done_groups && !strcmp(nm, "groups")) {
        if (TYPEOF(obj) != VECSXP)
          Rf_error("spec's 'groups' field must be a list");
        groups = obj;
        done_groups = true;
      } else if (!done_exclude && !strcmp(nm, "exclude")) {
        if (TYPEOF(obj) != STRSXP)
          Rf_error("spec's 'exclude' field must be a character vector");
        spec.exclude = obj;
        done_exclude = true;
      } else if (!done_include && !strcmp(nm, "include")) {
        if (TYPEOF(obj) != STRSXP)
          Rf_error("spec's 'include' field must be a character vector");
        spec.include = obj;
        done_include = true;
      } else if (!done_dedupe && !strcmp(nm, "dedupe")) {
        if (obj == R_NilValue) {
          spec.dedupe = Spec::Dedupe::INHERIT;
        } else {
          if (TYPEOF(obj) != LGLSXP || XLENGTH(obj) != 1)
            Rf_error("spec's 'dedupe' field must be a logical vector of length 1");
          spec.dedupe = (LOGICAL(obj)[0]) ?
            Spec::Dedupe::TRUE :
            Spec::Dedupe::FALSE;
        }
        done_dedupe = true;
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
  if (groups != R_NilValue) {
    R_xlen_t NG = XLENGTH(groups);
    SEXP gnames = Rf_getAttrib(groups, R_NamesSymbol);
    if (gnames == R_NilValue)
      Rf_error("groups must be a named list");
    spec.groups.reserve(NG);
    for (R_xlen_t g = 0; g < NG; g++) {
      const tuple<SEXP, vector<Spec>> gr =
        spec_group(STRING_ELT(gnames, g), VECTOR_ELT(groups, g));
      spec.groups.push_back(gr);
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

tuple<SEXP, vector<Spec>> spec_group(SEXP name, SEXP obj) {
  vector<Spec> specs;
  if (TYPEOF(obj) != VECSXP)
    Rf_error("Spec group must be an `unnest.spec` or a list of `unnest.spec`s");
  if (isSpec(obj)) {
    specs.push_back(list2spec(obj));
  } else {
    size_t N = XLENGTH(obj);
    for (size_t i = 0; i < N; i++) {
      SEXP s = VECTOR_ELT(obj, i);
      if (!isSpec(s))
        Rf_error("Each element of a group must be a spec. Not true for '%s'", CHAR(name));
      specs.push_back(list2spec(s));
    }
  }
  return tuple<SEXP, vector<Spec>>(name, specs);
}