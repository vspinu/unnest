
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
  SEXP include = R_NilValue;
  SEXP exclude = R_NilValue;
  int ix = -1;
  vector<Spec> children;
  vector<tuple<SEXP, vector<Spec>>> groups;
  bool stack = false;
  SEXP ix_name = R_NilValue;

  Spec(): node(R_NilValue), name(R_NilValue) {};
  Spec(SEXP node, SEXP name): node(node), name(name) {};

  vector<SpecMatch> match(SEXP obj) const;

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
