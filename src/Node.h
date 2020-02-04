
#ifndef UNNEST_NODE_H
#define UNNEST_NODE_H

#include <tuple>
#include <cstring>
#include <sstream>
#include "common.h"

struct SpecMatch {
  SEXP name, obj;
  SpecMatch(SEXP name, SEXP obj): name(name), obj(obj) {};
};

class Spec {
 public:
  SEXP node = R_NilValue;
  SEXP name = R_NilValue;
  int ix = -1;
  vector<Spec> children;
  bool stack = false;

  Spec(): node(R_NilValue), name(R_NilValue) {};
  Spec(SEXP node, SEXP name): node(node), name(name) {};

  vector<SpecMatch> match(SEXP obj) const {
    int N = LENGTH(obj);
    vector<SpecMatch> out;
    SEXP obj_names = Rf_getAttrib(obj, R_NamesSymbol);
    bool has_names = obj_names != R_NilValue;

    if (ix >= 0) {
      // 1) ix has total priority
      if (ix < N) {
        SEXP nm = (has_names ? R_NilValue : STRING_ELT(obj_names, ix));
        out.emplace_back(nm, VECTOR_ELT(obj, ix));
      }
    } else if (node == R_NilValue) {
      // 2) NULL node matches all
      out.reserve(N);
      for (size_t i = 0; i < N; i++) {
        SEXP nm = (has_names ? STRING_ELT(obj_names, i) : R_BlankString);
        out.emplace_back(nm, VECTOR_ELT(obj, i));
      }
    } else if (has_names) {
      // 3) Exact node match
      for (size_t i = 0; i < N; i++) {
        if (STRING_ELT(obj_names, i) == node) {
          out.emplace_back(name, VECTOR_ELT(obj, i));
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
      " stack:" << (stack ? "TRUE" : "FALSE") <<
      "]";
    return stream.str();
  }
};

const Spec NilSpec = Spec(R_NilValue, R_NilValue);

inline bool isSpec(SEXP s)
{
    SEXP cls;
    if (OBJECT(s)) {
      cls = getAttrib(s, R_ClassSymbol);
      for (int i = 0; i < LENGTH(cls); i++)
	    if (!strcmp(CHAR(STRING_ELT(cls, i)), "unnest.spec"))
          return true;
    }
    return false;
}


class Node {
 public:
  uint_fast32_t ix;
  virtual R_xlen_t size() const = 0;
  virtual SEXPTYPE type() const = 0;
  virtual void copy_into(SEXP target, R_xlen_t start, R_xlen_t end) const = 0;
  virtual ~Node() {};

 protected:
  Node (uint_fast32_t ix): ix(ix) {};
};

class SexpNode: public Node {
 public:
  SEXP obj;
  SexpNode(uint_fast32_t ix, SEXP obj): Node(ix), obj(obj) {};
  R_xlen_t size() const override {
    return XLENGTH(obj);
  }
  SEXPTYPE type() const override {
    return TYPEOF(obj);
  }
  void copy_into(SEXP target, R_xlen_t start, R_xlen_t end) const override {
    P("sexp copy of %ld: type:%s, start:%ld, end:%ld\n",
      ix, Rf_type2char(TYPEOF(target)), start, end);
    fill_vector(obj, target, start, end);
  }
};

class RangeNode: public Node {

  R_xlen_t _size = 0;
  SEXPTYPE _type = NILSXP;

 public:

  vector<tuple<R_xlen_t, R_xlen_t, unique_ptr<Node>>> pnodes;

  RangeNode(uint_fast32_t ix): Node(ix) {};
  RangeNode(uint_fast32_t ix, R_xlen_t size): Node(ix), _size(size) {};

  void push(R_xlen_t start, R_xlen_t end, unique_ptr<Node> pnode) {
    if (pnodes.size() == 0) {
      _type = pnode->type();
    } else {
      if (_type != pnode->type()) {
        _type = VECSXP;
      }
    }
    pnodes.emplace_back(start, end, move(pnode));
  }

  R_xlen_t size() const override {
    return _size;
  }

  void set_size(R_xlen_t size) {
    _size = size;
  }

  SEXPTYPE type() const override {
    return _type;
  }

  void copy_into(SEXP target, R_xlen_t beg, R_xlen_t end) const override {
    size_t N = pnodes.size();
    P("range copy of %ld: type:%s, beg:%ld, end:%ld Nnodes:%ld\n",
      ix, Rf_type2char(TYPEOF(target)), beg, end, N);
	for (R_xlen_t beg1 = beg; beg1 < end; beg1 += _size) {
      for (size_t n = 0; n < N; n++) {
        const auto& t = pnodes[n];
        const unique_ptr<Node>& p = get<2>(t);
        R_xlen_t
          beg2 = beg1 + get<0>(t),
          end2 = beg1 + get<1>(t);
        P("  n:%ld beg2:%ld end2:%ld\n", n, beg2, end2);
        p->copy_into(target, beg2, end2);
      }
    }
  }
};

#endif // UNNEST_H
