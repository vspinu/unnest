#include <string>
#include <deque>
#include <utility>
#include <forward_list>
#include <unordered_map>
#include <Rinternals.h>

using namespace std;

/// utilities

#define REP(N, len, i, j, body) do {              \
	i = j = 0;                                    \
	for (; i < N; j = (++j < len) ? j : 0, ++i) { \
	  body                                        \
	}                                             \
  } while (0)

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

/// Unnester

struct Node {
  uint_fast32_t iname;
  SEXP obj;

  Node(uint_fast32_t iname, SEXP obj): iname(iname), obj(obj) {};
};

class Unnester {

 private:

  string delimiter = ".";

  pair2ix_map p2i;
  ix2pair_map i2p;

  // conventions:
  //  iname: index name
  //  pname: pair name, <iname, char* name>
  uint_fast32_t next_iname = 1;

  void add_nodes(deque<Node>& els, SEXP x, uint_fast32_t parent_iname) {

	SEXP names = Rf_getAttrib(x, R_NamesSymbol);
	if (names == R_NilValue)
	  Rf_error("Unnesting unnamed list elements is not yet implemented");

	R_xlen_t N = XLENGTH(x);

	for (R_xlen_t i = 0; i < N; i++) {

	  const char* cname = CHAR(STRING_ELT(names, i));
	  auto pname = make_pair(parent_iname, cname);
	  uint_fast32_t iname;

	  pair2ix_map::iterator pnameit = p2i.find(pname);
	  if (pnameit == p2i.end()) {
		iname = next_iname++;
		p2i.insert(make_pair(pname, iname));
		i2p.insert(make_pair(iname, pname));
	  } else {
		iname = pnameit->second;
	  }

	  SEXP el = VECTOR_ELT(x, i);

	  if (XLENGTH(el) > 0)  {
		if (TYPEOF(el) == VECSXP) {
		  add_nodes(els, el, iname);
		} else {
		  els.push_front(Node(iname, el));
		}
	  }
	}

  }

  string full_name(uint_fast32_t iname) {

	if (iname == 0)
	  return "";

	forward_list<const char*> acc;
	ix2pair_map::iterator pit;

	do {
	  pit = i2p.find(iname);
	  if (pit == i2p.end())
		Rf_error("[Bug] Iname not in the hashmap, please report");
	  iname = pit->second.first;
	  acc.push_front(pit->second.second);
	} while (iname != 0);

	string out = acc.front();
	acc.pop_front();

	while (!acc.empty()) {
	  out.append(delimiter).append(acc.front());
	  acc.pop_front();
	}
	return out;
  }

  static SEXP rep(SEXP x, R_xlen_t N) {

	R_xlen_t i, j, n = XLENGTH(x);
	if (n == 0)
	  Rf_error("[Bug] Cannot replicate empty vector");

	SEXP out = PROTECT(allocVector(TYPEOF(x), N));

	switch (TYPEOF(x)) {
	 case LGLSXP:
	   REP(N, n, i, j, LOGICAL(out)[i] = LOGICAL(x)[j];); break;
	 case INTSXP:
	   REP(N, n, i, j, INTEGER(out)[i] = INTEGER(x)[j];); break;
	 case REALSXP:
	   REP(N, n, i, j, REAL(out)[i] = REAL(x)[j];); break;
	 case CPLXSXP:
	   REP(N, n, i, j, COMPLEX(out)[i] = COMPLEX(x)[j];); break;
	 case RAWSXP:
	   REP(N, n, i, j, RAW(out)[i] = RAW(x)[j];); break;
	 case STRSXP:
	   REP(N, n, i, j, SET_STRING_ELT(out, i, STRING_ELT(x, j));); break;
	 case VECSXP:
	 case EXPRSXP:
	   REP(N, n, i, j, SET_VECTOR_ELT(out, i, lazy_duplicate(VECTOR_ELT(x, j))););
	   break;
	 default:
	   Rf_error("Cannot unnest lists with elements of type %s", Rf_type2char(TYPEOF(x)));
	}

	Rf_setAttrib(out, R_ClassSymbol, Rf_getAttrib(x, R_ClassSymbol));

	if (Rf_inherits(x, "factor")) {
	  setAttrib(out, R_LevelsSymbol, Rf_getAttrib(x, R_LevelsSymbol));
	}

	UNPROTECT(1);
	return out;
  }

 public:

  SEXP process(SEXP x) {

	deque<Node> nodes;
	add_nodes(nodes, x, 0);

	SEXP out = PROTECT(Rf_allocVector(VECSXP, nodes.size()));
	SEXP names = PROTECT(Rf_allocVector(STRSXP, nodes.size()));

	R_xlen_t i = 0, max_len = 1, protects = 3;

	for (auto& node: nodes) {
	  // Cross-product.
	  // All are non-empty vectors.
	  max_len = XLENGTH(node.obj) * max_len;
	}

	for (auto& node : nodes) {
	  string name = full_name(node.iname);
	  R_xlen_t this_len = XLENGTH(node.obj);
	  SEXP obj;
	  if (this_len == max_len) {
		obj = node.obj;
	  } else {
		obj = PROTECT(rep(node.obj, max_len));
		protects++;
	  }
	  SET_VECTOR_ELT(out, i, obj);
	  SET_STRING_ELT(names, i, Rf_mkCharLenCE(name.c_str(), name.size(), CE_UTF8));
	  i++;
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
