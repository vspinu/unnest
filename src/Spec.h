
#ifndef UNNEST_SPEC_H
#define UNNEST_SPEC_H

#include "common.h"
#include "unordered_map"

struct SpecMatch {
  int ix = -1;
  SEXP spec_name, elem_name, obj;
  SpecMatch(int ix, SEXP spec_name, SEXP obj):
    ix(ix), spec_name(spec_name), obj(obj) {};
  SpecMatch(int ix, SEXP spec_name, SEXP elem_name, SEXP obj):
    ix(ix), spec_name(spec_name), elem_name(elem_name), obj(obj) {};

  string to_string() const {
    std::ostringstream stream;
    stream << "match[ix:" << ix <<
      " spec_name:" << (spec_name == R_NilValue ? "NULL" : CHAR(spec_name)) <<
      " elem_name:" << (elem_name == R_NilValue ? "NULL" : CHAR(elem_name)) << "]";
    return stream.str();
  }
};

struct Spec {
  enum Dedupe {INHERIT, TRUE, FALSE};
  const std::unordered_map<Dedupe, string> dedupe_names = {
    {INHERIT, "INHERIT"},
    {TRUE, "TRUE"},
    {FALSE, "FALSE"}
  };
  Dedupe dedupe = INHERIT;

  bool terminal = true;

  SEXP name = R_NilValue;
  vector<int> include_ixes;
  vector<SEXP> include_names;
  vector<int> exclude_ixes;
  vector<SEXP> exclude_names;

  vector<Spec> children;
  vector<tuple<SEXP, vector<Spec>>> groups;
  bool stack = false;
  SEXP ix_name = R_NilValue;

  Spec(): name(R_NilValue) {};
  Spec(SEXP name): name(name) {};

  vector<SpecMatch> match(SEXP obj) const;

  void set_terminal() {
    terminal =
      include_ixes.size() == 0 &&
      include_names.size() == 0 &&
      exclude_ixes.size() == 0 &&
      exclude_names.size() == 0;
    // FIXME: add groups?
    for (const Spec& sp: children) {
      terminal = terminal && sp.terminal;
    }
  }

  string to_string() const {
    std::ostringstream stream;
    stream << "[spec:" <<
      " name:" << (name == R_NilValue ? "NULL" : CHAR(name)) <<
      " stack:" << (stack ? "TRUE" : "FALSE") <<
      " dedupe:" << dedupe_names[dedupe].c_str() <<
      " terminal:" << (terminal ? "TRUE" : "FALSE") <<
      "]";
    return stream.str();
  }

};

Spec list2spec(SEXP lspec);
bool isSpec(SEXP s);
tuple<SEXP, vector<Spec>> spec_group(SEXP name, SEXP obj);

const Spec NilSpec = Spec(R_NilValue);

#endif // UNNEST_SPEC_H
