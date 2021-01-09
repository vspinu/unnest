
#include <algorithm>
#include "common.h"
#include "Spec.h"

Spec::Stack sexp2stack(SEXP x) {
  if (x == R_NilValue)
    return Spec::Stack::AUTO;
  if (TYPEOF(x) == LGLSXP) {
    if (LOGICAL(x)[0])
      return Spec::Stack::STACK;
    else
      return Spec::Stack::SPREAD;
  }
  if (TYPEOF(x) == STRSXP) {
    return Spec::Stack::STACK;
  }
  Rf_error("Invalid `stack` argument. Must be TRUE, FALSE, NULL, or a string scalar");
}

Spec::Process sexp2process(SEXP x) {
  if (x == R_NilValue)
    return Spec::Process::NONE;
  if (TYPEOF(x) == STRSXP) {
    const char* nm = CHAR(STRING_ELT(x, 0));
    if (!strcmp(nm, "asis"))
      return Spec::Process::ASIS;
    else if (!strcmp(nm, "paste"))
      return Spec::Process::PASTE;
    else if (!strcmp(nm, "paste_strings"))
      return Spec::Process::PASTE_STRING;
  }
  // don't popularize paste_strings as yet. Not very useful in practice as
  // string vectors come as lists usually.
  Rf_error("Invalid `process` or `process_atomic` argument. Must be one of 'asis', 'paste' or NULL");
}

vector<SpecMatch> Spec::match(SEXP obj) const {
  int N = LENGTH(obj);
  SEXP obj_names = Rf_getAttrib(obj, R_NamesSymbol);
  bool has_names = obj_names != R_NilValue;

  vector<SpecMatch> out;

  // NULL node matches all
  if (include_names.size() == 0 && exclude_names.size() == 0 &&
      include_ixes.size() == 0 && exclude_ixes.size() == 0) {
    // fixme: figure out a way not to create a full extent matcher in this case
    out.reserve(N);
    for (int i = 0; i < N; i++) {
      SEXP nm = has_names ? STRING_ELT(obj_names, i) : R_NilValue;
      out.emplace_back(i, name, nm, VECTOR_ELT(obj, i));
    }
    return out;
  }

  if (include_ixes.size() > 0) {
    for (int ix : include_ixes) {
      if (ix < N) {
        if (exclude_ixes.size() > 0 &&
            std::find(exclude_ixes.begin(), exclude_ixes.end(), ix) != exclude_ixes.end())
          continue;
        SEXP nm = has_names ? STRING_ELT(obj_names, ix) : R_NilValue;
        out.emplace_back(ix, name, nm, VECTOR_ELT(obj, ix));
      } else {
        if (defsexp != R_NilValue) {
          out.emplace_back(ix, name, R_NilValue, defsexp);
        }
      }
    }
  } else if (exclude_ixes.size() > 0) {
    out.reserve(N);
    for (int i = 0; i < N; i++) {
      if (std::find(exclude_ixes.begin(), exclude_ixes.end(), i) == exclude_ixes.end()) {
        SEXP nm = has_names ? STRING_ELT(obj_names, i) : R_NilValue;
        out.emplace_back(i, name, nm, VECTOR_ELT(obj, i));
      }
    }
  }

  if (has_names && (include_names.size() > 0 || exclude_names.size() > 0)) {
    if (exclude_names.size() > 0)
      out.reserve(N);

    vector<bool> processed(include_names.size(), false);

    for (int i = 0; i < N; i++) {
      SEXP nm = R_NilValue;
      nm = STRING_ELT(obj_names, i);
      if (ix_char_in_strvec(nm, exclude_names) < 0) {
        if (include_names.size() > 0) {
          R_xlen_t ix = ix_char_in_strvec(nm, include_names);
          if (ix >= 0) {
            out.emplace_back(i, name, nm, VECTOR_ELT(obj, i));
            processed[ix] = true;
          }
        } else {
          out.emplace_back(i, name, nm, VECTOR_ELT(obj, i));
        }
      }
    }

    if (defsexp != R_NilValue) {
      for (size_t i = 0; i < processed.size(); i++) {
        if (!processed[i]) {
          out.emplace_back(-1, name, include_names[i], defsexp);
        }
      }
    }
  }

  return out;
}

void fill_spec_ixes(const char* name, SEXP obj, vector<int>& int_ixes, vector<SEXP>& str_ixes) {
  R_xlen_t n = XLENGTH(obj);
  switch (TYPEOF(obj)) {
   case STRSXP:
     for (R_xlen_t i = 0; i < n; i++)
       str_ixes.push_back(STRING_ELT(obj, i));
     break;
   case INTSXP: ;
     for (R_xlen_t i = 0; i < n; i++)
       int_ixes.push_back(INTEGER(obj)[i] - 1);
     break;
   case REALSXP:
     for (R_xlen_t i = 0; i < n; i++)
       int_ixes.push_back(REAL(obj)[i] - 1);
     break;
   case VECSXP:
     for (R_xlen_t i = 0; i < n; i++)
       fill_spec_ixes(name, VECTOR_ELT(obj, i), int_ixes, str_ixes);;
     break;
   default:
     Rf_error("spec's '%s' field must be a character, numeric or a list", name);
  }
}

Spec sexp2spec(SEXP lspec) {
  if (LENGTH(lspec) == 0)
    return NilSpec;

  if (TYPEOF(lspec) != VECSXP)
    Rf_error("'spec' must be a list");
  if (!isSpec(lspec))
    Rf_error("'spec' must be of S3 class 'unnest.spec'");

  SEXP names = Rf_getAttrib(lspec, R_NamesSymbol);
  if (names == R_NilValue)
    Rf_error("unnest.spec must have non-nil names");

  R_xlen_t N = LENGTH(lspec);
  bool done_as = false,
    done_children = false, done_groups = false,
    done_stack = false, done_process = false,
    done_include = false, done_exclude = false,
    done_default = false;
  SEXP children = R_NilValue, groups = R_NilValue;

  Spec spec;

  for (R_xlen_t i = 0; i < N; i++) {
    SEXP obj = VECTOR_ELT(lspec, i);
    if (obj != R_NilValue) {
      const char* nm = CHAR(STRING_ELT(names, i));

      if (!done_as && !strcmp(nm, "as")) {
        if (TYPEOF(obj) != STRSXP || XLENGTH(obj) != 1)
          Rf_error("spec's 'as' field must be a scalar string");
        spec.name = STRING_ELT(obj, 0);
        done_as = true;
      } else if (!done_stack && !strcmp(nm, "stack")) {
        if (TYPEOF(obj) == STRSXP) {
          spec.ix_name = STRING_ELT(obj, 0);
        }
        spec.stack = sexp2stack(obj);
        done_stack = true;
      } else if (!done_process && !strcmp(nm, "process")) {
        spec.process = sexp2process(obj);
        done_process = true;
      } else if (!done_default && !strcmp(nm, "default")) {
        spec.defsexp = obj;
        done_default = true;
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
        fill_spec_ixes("exclude", obj, spec.exclude_ixes, spec.exclude_names);
        done_exclude = true;
      } else if (!done_include && !strcmp(nm, "include")) {
        fill_spec_ixes("include", obj, spec.include_ixes, spec.include_names);
        done_include = true;
      }
    }
  }

  if (spec.name != R_NilValue) {
    if (spec.stack != Spec::Stack::STACK && (spec.include_ixes.size() + spec.include_names.size()) > 1)
      Rf_error("Supplied 'as' value with multiple 'include' elements and `stack == FALSE`");
  }

  if (children != R_NilValue) {
    R_xlen_t NC = XLENGTH(children);
    spec.children.reserve(NC);
    for (R_xlen_t c = 0; c < NC; c++) {
      SEXP ch = VECTOR_ELT(children, c);
      spec.children.emplace_back(sexp2spec(ch));
    }
  }

  if (groups != R_NilValue) {
    R_xlen_t NG = XLENGTH(groups);
    SEXP gnames = Rf_getAttrib(groups, R_NamesSymbol);
    if (gnames == R_NilValue)
      Rf_error("groups must be a named list");
    spec.groups.reserve(NG);
    for (R_xlen_t g = 0; g < NG; g++) {
      tuple<SEXP, vector<Spec>> gr =
        spec_group(STRING_ELT(gnames, g), VECTOR_ELT(groups, g));
      spec.groups.push_back(gr);
    }
  }

  spec.set_terminal();

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
    specs.push_back(sexp2spec(obj));
  } else {
    size_t N = XLENGTH(obj);
    for (size_t i = 0; i < N; i++) {
      SEXP s = VECTOR_ELT(obj, i);
      if (!isSpec(s))
        Rf_error("Each element of a group must be a spec. Not true for '%s'", CHAR(name));
      specs.push_back(sexp2spec(s));
    }
  }
  return tuple<SEXP, vector<Spec>>(name, specs);
}
