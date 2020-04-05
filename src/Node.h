
#ifndef UNNEST_NODE_H
#define UNNEST_NODE_H

#include "common.h"

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

class ElNode: public Node {
 public:
  SEXP obj;
  size_t el;
  ElNode(uint_fast32_t ix, size_t el, SEXP obj): Node(ix), el(el), obj(obj) {};
  R_xlen_t size() const override {
    return 1;
  }
  SEXPTYPE type() const override {
    return TYPEOF(obj);
  }
  void copy_into(SEXP target, R_xlen_t start, R_xlen_t end) const override {
    P("el copy of node %ld: el:%ld type:%s, start:%ld, end:%ld\n",
      ix, el, Rf_type2char(TYPEOF(target)), start, end);
    if (TYPEOF(target) == TYPEOF(obj)) {
      fill_vector_1(obj, el, target, start, end);
    } else {
      // Raw element coercion is not exposed by Rinternals. So extract scalar first.
      SEXP obj1 = Rf_coerceVector(extract_scalar(obj, el), TYPEOF(target));
      fill_vector_1(obj1, 0, target, start, end);
    }
  }
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
    P("sexp copy of node %ld: type:%s, start:%ld, end:%ld\n",
      ix, Rf_type2char(TYPEOF(target)), start, end);
    if (TYPEOF(target) == TYPEOF(obj)) {
      fill_vector(obj, target, start, end);
    } else {
      SEXP obj1 = Rf_coerceVector(obj, TYPEOF(target));
      fill_vector(obj1, target, start, end);
    }
  }
};

class IxNode: public Node {

  R_xlen_t _size = 0;
  SEXPTYPE _type = NILSXP;
  vector<tuple<R_xlen_t, R_xlen_t, int>> _int_ixs;
  vector<tuple<R_xlen_t, R_xlen_t, SEXP>> _chr_ixs;

 public:

  IxNode(uint_fast32_t ix): Node(ix) {};

  void push(R_xlen_t start, R_xlen_t end, int ix, SEXP name) {
    if (name != R_NilValue)
      _chr_ixs.emplace_back(start, end, name);
    else
      _int_ixs.emplace_back(start, end, ix);
  }

  R_xlen_t size() const override {
    return _size;
  }

  void set_size(R_xlen_t size) {
    _size = size;
  }

  SEXPTYPE type() const override {
    return (_chr_ixs.size() == 0) ? INTSXP : STRSXP;
  }

  void copy_into_INTSXP(SEXP target, R_xlen_t beg, R_xlen_t end) const {
    size_t N = _int_ixs.size();
    int* IX = INTEGER(target);
    P("ix copy of %ld: beg:%ld, end:%ld N-ixes:%ld\n", ix, beg, end, N);
	for (R_xlen_t beg1 = beg; beg1 < end; beg1 += _size) {
      for (const auto& t: _int_ixs) {
        R_xlen_t
          beg2 = beg1 + get<0>(t),
          end2 = beg1 + get<1>(t);
        P("  <int>beg2:%ld end2:%ld\n", beg2, end2);
        for (R_xlen_t i = beg2; i < end2; i++) {
          IX[i] = get<2>(t);
        }
      }
    }
  }

  void copy_into_STRSXP(SEXP target, R_xlen_t beg, R_xlen_t end) const {
    P("ix copy of %ld: beg:%ld, end:%ld N-ixes:%ld\n", ix, beg, end, XLENGTH(target));
	for (R_xlen_t beg1 = beg; beg1 < end; beg1 += _size) {
      for (const auto& t: _chr_ixs) {
        R_xlen_t
          beg2 = beg1 + get<0>(t),
          end2 = beg1 + get<1>(t);
        P("  <str>beg2:%ld end2:%ld\n", beg2, end2);
        for (R_xlen_t i = beg2; i < end2; i++) {
          SET_STRING_ELT(target, i, get<2>(t));
        }
      }
      // convert integers to chars for the rare mixed int/str case
      for (const auto& t: _int_ixs) {
        R_xlen_t
          beg2 = beg1 + get<0>(t),
          end2 = beg1 + get<1>(t);
        P("  <int:str>beg2:%ld end2:%ld\n", beg2, end2);
        for (R_xlen_t i = beg2; i < end2; i++) {
          SET_STRING_ELT(target, i, Rf_mkChar(to_string(get<2>(t)).c_str()));
        }
      }
    }
  }

  void copy_into(SEXP target, R_xlen_t beg, R_xlen_t end) const override {
    size_t N = _int_ixs.size();
    if (TYPEOF(target) == INTSXP)
      copy_into_INTSXP(target, beg, end);
    else if (TYPEOF(target) == STRSXP)
      copy_into_STRSXP(target, beg, end);
    else Rf_error("Cannot copy an IxNode into a non INTSXP or non STRSXP target (%s)",
                  Rf_type2char(TYPEOF(target)));
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
    } else if (_type != STRSXP) {
      SEXPTYPE new_type = pnode->type();
      if (_type != new_type) {
        if (new_type == STRSXP)
          _type = STRSXP;
        else if (new_type == REALSXP)
          _type = REALSXP;
        else if (new_type == LGLSXP || new_type == INTSXP) {
          if (_type == LGLSXP)
            _type = INTSXP;
        } else {
          // fallback on character
          _type = STRSXP;
        }
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

#endif // UNNEST_NODE_H
