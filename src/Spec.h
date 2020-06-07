
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
  enum Stack {STACK, SPREAD, AUTO};
  const std::unordered_map<Stack, string> stack_names = {
    {STACK, "STACK"},
    {SPREAD, "SPREAD"},
    {AUTO, "AUTO"}
  };
  Stack stack = AUTO;

  bool terminal = true;
  bool terminal_parent = true;

  SEXP name = R_NilValue; //FIXME: rename into "as"
  string type = "";
  vector<int> include_ixes;
  vector<SEXP> include_names;
  vector<int> exclude_ixes;
  vector<SEXP> exclude_names;

  vector<Spec> children;
  vector<tuple<SEXP, vector<Spec>>> groups;
  SEXP ix_name = R_NilValue;

  Spec() {};
  Spec(string type): type(type) {};
  Spec(string type, bool terminal_parent): type(type), terminal_parent(terminal_parent) {};

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
    if (!terminal) {
      for (Spec& sp: children) {
        sp.terminal_parent = false;
      }
    }
  }

  string to_string() const {
    std::ostringstream stream;
    std::string name = this->type;
    for (SEXP nm: include_names) {
      name.append(CHAR(nm)).append(",");
    }
    stream << "[spec:" << name <<
      " stack:" << stack_names.at(stack).c_str() <<
      " terminal[parent]:" << (terminal ? "T" : "F") <<
      "[" << (terminal_parent ? "T" : "F") << "]" <<
      "]";
    return stream.str();
  }

};


bool isSpec(SEXP s);
Spec::Stack sexp2stack(SEXP x);
Spec list2spec(SEXP lspec);
tuple<SEXP, vector<Spec>> spec_group(SEXP name, SEXP obj);

const Spec NilSpec = Spec("NIL");
const Spec LeafSpec = Spec("LEAF", false);

#endif // UNNEST_SPEC_H
