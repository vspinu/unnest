#include <vector>
#include <string>
#include <deque>
#include <utility>
#include <forward_list>
#include <unordered_map>
#include "unnest.h"

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

struct Node {
  uint_fast32_t ix;
  SEXP obj;

  Node(uint_fast32_t ix, SEXP obj): ix(ix), obj(obj) {};
};

class Unnester {

 private:

  string delimiter = ".";
  pair2ix_map p2i;
  ix2pair_map i2p;

  vector<string> num_cache;

  uint_fast32_t next_ix = 1;

  void add_nodes(deque<Node>& els, SEXP x, uint_fast32_t parent_ix) {

	R_xlen_t N = XLENGTH(x);

	SEXP names = Rf_getAttrib(x, R_NamesSymbol);
	bool has_names = names != R_NilValue;
	if (!has_names) {
	  if (num_cache.size() < N) {
		num_cache.reserve(N);
		for (R_xlen_t i = num_cache.size(); i < N; i++) {
		  num_cache.push_back(to_string(i+1));
		}
	  }
	}

	for (R_xlen_t i = 0; i < N; i++) {

	  const char* cname =
		has_names ? CHAR(STRING_ELT(names, i)) : num_cache[i].c_str();

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

	  SEXP el = VECTOR_ELT(x, i);

	  if (XLENGTH(el) > 0)  {
		if (TYPEOF(el) == VECSXP) {
		  add_nodes(els, el, ix);
		} else {
		  els.push_front(Node(ix, el));
		}
	  }
	}

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


 public:

  SEXP process(SEXP x) {

	deque<Node> nodes;
	add_nodes(nodes, x, 0);

	SEXP out = PROTECT(Rf_allocVector(VECSXP, nodes.size()));
	SEXP names = PROTECT(Rf_allocVector(STRSXP, nodes.size()));

	R_xlen_t
	  i = nodes.size() - 1,
	  max_len = 1,
	  protects = 3;

	for (Node& node: nodes) {
	  // Cross-product.
	  // All are non-empty vectors.
	  max_len = XLENGTH(node.obj) * max_len;
	}

	for (Node& node : nodes) {
	  string name = full_name(node.ix);
	  R_xlen_t this_len = XLENGTH(node.obj);
	  SEXP obj;
	  if (this_len == max_len) {
		obj = node.obj;
	  } else {
		obj = PROTECT(rep_vector(node.obj, max_len));
		protects++;
	  }
	  SET_VECTOR_ELT(out, i, obj);
	  SET_STRING_ELT(names, i, Rf_mkCharLenCE(name.c_str(), name.size(), CE_UTF8));
	  i--;
	}

	// build data.frame
	SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
	INTEGER(row_names)[0] = NA_INTEGER;
	INTEGER(row_names)[1] = -max_len;
	Rf_setAttrib(out, R_ClassSymbol, ScalarString(mkChar("data.frame")));
	Rf_setAttrib(out, R_RowNamesSymbol, row_names);
	Rf_setAttrib(out, R_NamesSymbol, names);
	UNPROTECT(protects);

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
