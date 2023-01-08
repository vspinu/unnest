
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

  std::string to_string() const {
    std::ostringstream stream;
    stream << "match[ix:" << ix <<
      " spec_name:" << (spec_name == R_NilValue ? "NULL" : CHAR(spec_name)) <<
      " elem_name:" << (elem_name == R_NilValue ? "NULL" : CHAR(elem_name)) << "]";
    return stream.str();
  }
};

struct Spec {
  enum Stack {STACK, SPREAD, AUTO};
  enum Process {ASIS, PASTE, PASTE_STRING, NONE};
#if __GNUC__ > 5
  const std::unordered_map<Process, std::string> process_names = {
    {ASIS, "ASIS"}, {PASTE, "PASTE"}, {PASTE_STRING, "PASTE_STRING"}, {NONE, "NONE"}
  };
  const std::unordered_map<Stack, std::string> stack_names = {
    {STACK, "STACK"}, {SPREAD, "SPREAD"}, {AUTO, "AUTO"}
  };
#endif
  Stack stack = AUTO;
  Process process = NONE;

  bool terminal = true;

  SEXP name = R_NilValue; //FIXME: rename into "as"
  SEXP defsexp = R_NilValue;
  std::string type = "";
  std::vector<int> include_ixes;
  std::vector<SEXP> include_names;
  std::vector<int> exclude_ixes;
  std::vector<SEXP> exclude_names;

  std::vector<Spec> children;
  std::vector<std::tuple<SEXP, std::vector<Spec>>> groups;
  SEXP ix_name = R_NilValue;

  Spec() {};
  Spec(std::string type): type(type) {};

  std::vector<SpecMatch> match(SEXP obj) const;

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

  std::string to_string() const {
    std::ostringstream stream;
    std::string name = this->type;
    for (SEXP nm: include_names) {
      name.append(CHAR(nm)).append(",");
    }
    stream << "spec[" << name <<
#if __GNUC__ > 5
      " stack:" << stack_names.at(stack).c_str() <<
      " process:" << process_names.at(process).c_str() <<
#endif
      " terminal[parent]:" << (terminal ? "T" : "F") <<
      "]";
    return stream.str();
  }

};


bool isSpec(SEXP s);
Spec::Stack sexp2stack(SEXP x);
Spec::Process sexp2process(SEXP x);
Spec sexp2spec(SEXP lspec);
std::tuple<SEXP, std::vector<Spec>> spec_group(SEXP name, SEXP obj);

const Spec NilSpec = Spec("NIL");

#endif // UNNEST_SPEC_H
