
#ifndef UNNEST_SPEC_H
#define UNNEST_SPEC_H

#include "common.h"

struct SpecMatch {
  int ix = -1;
  SEXP name, obj;
  SpecMatch(int ix, SEXP name, SEXP obj): ix(ix), name(name), obj(obj) {};

  string to_string() const {
    std::ostringstream stream;
    stream << "spec[ix:" << ix << " name:" << (name == R_NilValue ? "NULL" : CHAR(name)) << "]";
    return stream.str();
  }
};

struct Spec {
  enum Dedupe {INHERIT, TRUE, FALSE};
  Dedupe dedupe = INHERIT;
  SEXP node = R_NilValue;
  SEXP name = R_NilValue;
  SEXP exclude = R_NilValue;
  int ix = -1;
  vector<Spec> children;
  vector<tuple<SEXP, vector<Spec>>> groups;
  bool stack = false;

  Spec(): node(R_NilValue), name(R_NilValue) {};

  Spec(SEXP node, SEXP name): node(node), name(name) {};

  vector<SpecMatch> match(SEXP obj) const {
    int N = LENGTH(obj);
    vector<SpecMatch> out;
    SEXP obj_names = Rf_getAttrib(obj, R_NamesSymbol);
    bool has_names = obj_names != R_NilValue;

    if (ix >= 0) {
      // 1) ix has the highest priority
      if (ix < N) {
        SEXP nm = R_NilValue;
        if (name != R_NilValue)
          nm = name;
        else if (has_names)
          nm = STRING_ELT(obj_names, ix);
        out.emplace_back(ix, nm, VECTOR_ELT(obj, ix));
      }
    } else if (node == R_NilValue) {
      // 2) NULL node matches all
      out.reserve(N);
      for (int i = 0; i < N; i++) {
        SEXP nm = R_NilValue;
        if (has_names) {
          nm = STRING_ELT(obj_names, i);
          if (is_char_in_strvec(nm, exclude)) {
            continue;
          }
        }
        out.emplace_back(i, nm, VECTOR_ELT(obj, i));
      }
    } else if (has_names) {
      // 3) Exact node match
      for (size_t i = 0; i < N; i++) {
        if (STRING_ELT(obj_names, i) == node) {
          out.emplace_back(i, name, VECTOR_ELT(obj, i));
          break;
        }
      }
    }

    return out;
  }

  string to_string() const {
    std::ostringstream stream;
    stream << "[node:" <<
      (node == R_NilValue ? "NULL" : CHAR(node)) <<
      " name:" << (name == R_NilValue ? "NULL" : CHAR(name)) <<
      " ix: " << ix <<
      " stack:" << (stack ? "TRUE" : "FALSE") <<
      "]";
    return stream.str();
  }

};

Spec list2spec(SEXP lspec);
bool isSpec(SEXP s);

tuple<SEXP, vector<Spec>> spec_group(SEXP name, SEXP obj);

const Spec NilSpec = Spec(R_NilValue, R_NilValue);
const Spec LeafSpec = Spec(R_NilValue, R_NilValue);


#endif // UNNEST_SPEC_H
