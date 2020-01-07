#include <vector>
#include <string>
#include <deque>
#include <tuple>
#include <memory>
#include <algorithm>
#include <utility>
#include <forward_list>
#include <unordered_map>
#include <unordered_set>
#include "unnest.h"

#undef nrows

using namespace std;

struct hash_pair {
  template <class T1, class T2>
  size_t operator()(const pair<T1, T2>& p) const
  {
	auto hash1 = hash<T1>{}(p.first);
	auto hash2 = hash<T2>{}(p.second);
	return hash1 ^ hash2;
  }
};

typedef unordered_map<pair<uint_fast32_t, const char*>, uint_fast32_t, hash_pair> pair2ix_map;
typedef unordered_map<uint_fast32_t, pair<uint_fast32_t, const char*>> ix2pair_map;

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
    P("sexp copy: type:%s, start:%ld, end:%ld\n",
      Rf_type2char(TYPEOF(target)), start, end);
    fill_vector(obj, target, start, end);
  }
};

class RangeNode: public Node {

  R_xlen_t _size = 0;
  SEXPTYPE _type = NILSXP;
  vector<tuple<R_xlen_t, R_xlen_t, unique_ptr<Node>>> range_pnodes;

 public:

  RangeNode(uint_fast32_t ix): Node(ix) {};
  RangeNode(uint_fast32_t ix, R_xlen_t size): Node(ix), _size(size) {};

  void push(R_xlen_t start, R_xlen_t end, unique_ptr<Node> pnode) {
    if (range_pnodes.size() == 0) {
      _type = pnode->type();
    } else {
      if (_type != pnode->type()) {
        _type = VECSXP;
      }
    }
    range_pnodes.emplace_back(start, end, move(pnode));
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

  void copy_into(SEXP target, R_xlen_t start, R_xlen_t end) const override {
    P("range copy: type:%s, start:%ld, end:%ld\n",
      Rf_type2char(TYPEOF(target)), start, end);
    R_xlen_t N = range_pnodes.size();
    vector<R_xlen_t> sizes;
    sizes.reserve(N);
    for (size_t i = 0; i < N; i++) {
      sizes[i] = (get<2>(range_pnodes[i]))->size();
    }
	for (R_xlen_t i = start, n = 0; n < N && i < end; n++) {
      const auto& t = range_pnodes[n];
      const unique_ptr<Node>& p = get<2>(t);
      R_xlen_t nexti = i + get<1>(t) - get<0>(t);
      p->copy_into(target, i, nexti);
      i = nexti;
      if (++n == N)
        n = 0;
    }
  }
};



struct NodeAccumulator {
  R_xlen_t nrows = 1;
  deque<unique_ptr<Node>> pnodes;
};


class Unnester {

 private:

  pair2ix_map p2i;
  ix2pair_map i2p;

  vector<string> num_cache;
  string delimiter = ".";
  uint_fast32_t next_ix = 1;

  void populate_num_cache(R_xlen_t N) {
    if (num_cache.size() < N) {
      num_cache.reserve(N);
      for (R_xlen_t i = num_cache.size(); i < N; i++) {
        num_cache.push_back(to_string(i+1));
      }
    }
  }

  uint_fast32_t child_ix(uint_fast32_t parent_ix, const char* cname) {
    //  pname: aka pair name, <parent ix, char* this name>
    auto pname = make_pair(parent_ix, cname);
    uint_fast32_t ix;

    pair2ix_map::iterator pnameit = p2i.find(pname);
    if (pnameit == p2i.end()) {
      ix = next_ix++;
      p2i.insert(make_pair(pname, ix));
      i2p.insert(make_pair(ix, pname));
    } else {
      ix = pnameit->second;
    }
    return ix;
  }

  string full_name(uint_fast32_t ix) {

	if (ix == 0)
	  return "";

	forward_list<const char*> acc;
	ix2pair_map::iterator pit;

	do {
	  pit = i2p.find(ix);
	  if (pit == i2p.end())
		Rf_error("[Bug] Iname not in the hashmap, please report");
	  ix = pit->second.first;
	  acc.push_front(pit->second.second);
	} while (ix != 0);

	string out = acc.front();
	acc.pop_front();

	while (!acc.empty()) {
	  out.append(delimiter).append(acc.front());
	  acc.pop_front();
	}
	return out;
  }

  inline void add_node(NodeAccumulator& acc, SEXP x, uint_fast32_t ix) {
    R_xlen_t N = XLENGTH(x);
    bool stack = Rf_getAttrib(x, R_NamesSymbol) == R_NilValue;

    if (N > 0) {
      if (TYPEOF(x) == VECSXP) {
        if (stack) {
          add_stacked_nodes(acc, x, ix);
        } else {
          add_nodes(acc, x, ix);
        }
        P("added vec node:%s\n", full_name(ix).c_str());
      } else {
        acc.pnodes.push_front(make_unique<SexpNode>(ix, x));
        acc.nrows *= N;
        P("added sexp node:%s\n", full_name(ix).c_str());
      }
    }

    P("added node acc size:%ld  nrow:%ld\n", acc.pnodes.size(), acc.nrows);

  }

  void add_nodes(NodeAccumulator& acc, SEXP& x, uint_fast32_t parent_ix) {

	R_xlen_t N = XLENGTH(x);

	SEXP names = Rf_getAttrib(x, R_NamesSymbol);
	bool has_names = names != R_NilValue;
	if (!has_names) {
      populate_num_cache(N);
	}

	for (R_xlen_t i = 0; i < N; i++) {

      const char* cname =
		has_names ? CHAR(STRING_ELT(names, i)) : num_cache[i].c_str();

	  add_node(acc,
               VECTOR_ELT(x, i),
               child_ix(parent_ix, cname));
	}

    P("added nodes acc size:%ld  nrow:%ld\n", acc.pnodes.size(), acc.nrows);

  }

  void add_stacked_nodes(NodeAccumulator& acc, SEXP x, uint_fast32_t parent_ix) {
    R_xlen_t N = XLENGTH(x);

    R_xlen_t beg = 0, end=0;

    unordered_map<uint_fast32_t, unique_ptr<RangeNode>> out_nodes;

    for (R_xlen_t i = 0; i < N; i++) {

      NodeAccumulator iacc;
      add_node(iacc,
               VECTOR_ELT(x, i),
               parent_ix);
      end += iacc.nrows;

      // move to out_nodes
      while (!iacc.pnodes.empty()) {
        unique_ptr<Node>& ip = iacc.pnodes.front();
        auto oit = out_nodes.find(ip->ix);
        if (oit == out_nodes.end()) {
          unique_ptr<RangeNode> pr = make_unique<RangeNode>(ip->ix);
          pr->push(beg, end, move(ip));
          P("stacking node:%s type:%s\n", full_name(pr->ix).c_str(), Rf_type2char(pr->type()));
          out_nodes.emplace(pr->ix, move(pr));
        } else {
          oit->second->push(beg, end, move(ip));
        }
        iacc.pnodes.pop_front();
      }
      beg = end;
    }

    for (auto& on: out_nodes) {
      on.second->set_size(end);
      P("stacked node:%s type:%s, size:%ld\n",
        full_name(on.second->ix).c_str(), Rf_type2char(on.second->type()), on.second->size());
      acc.pnodes.push_front(move(on.second));
    }

    acc.nrows *= end;

    P("stacked acc size:%ld  nrow:%ld\n", acc.pnodes.size(), acc.nrows);
  }



 public:

  SEXP process(SEXP x) {

	NodeAccumulator acc;
	add_nodes(acc, x, 0);

    P("FINAL: nrow:%ld ncol:%ld\n", acc.nrows, acc.pnodes.size());
    SEXP out = PROTECT(Rf_allocVector(VECSXP, acc.pnodes.size()));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, acc.pnodes.size()));

    R_xlen_t i = acc.pnodes.size() - 1;

	for (unique_ptr<Node>& p: acc.pnodes) {
      P("alloc type: %s\n", Rf_type2char(p->type()));
      if (p->type() == VECSXP)
        Rf_error("Cannot handle irregular types yet");
	  /* SEXP obj = Rf_allocVector(p->type(), acc.nrows); */
      SEXP obj = make_na_vector(p->type(), acc.nrows);
	  SET_VECTOR_ELT(out, i, obj);
      p->copy_into(obj, 0, acc.nrows);
      string name = full_name(p->ix);
	  SET_STRING_ELT(names, i, Rf_mkCharLenCE(name.c_str(), name.size(), CE_UTF8));
	  i--;
	}

	// build data.frame
	SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
	INTEGER(row_names)[0] = NA_INTEGER;
	INTEGER(row_names)[1] = acc.nrows;
	Rf_setAttrib(out, R_ClassSymbol, ScalarString(mkChar("data.frame")));
	Rf_setAttrib(out, R_RowNamesSymbol, row_names);
	Rf_setAttrib(out, R_NamesSymbol, names);
	UNPROTECT(3);

	return out;
  }
};

extern "C" SEXP C_unnest(SEXP x) {
  SEXPTYPE type = TYPEOF(x);
  if (TYPEOF(x) != VECSXP) {
	Rf_error("x must be a list vector");
  }

  Unnester unnester;

  return unnester.process(x);
}
