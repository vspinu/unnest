#include <vector>
#include <string>
#include <deque>
#include <utility>
#include <forward_list>
#include <unordered_map>
#include <unordered_set>
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

  pair2ix_map p2i;
  ix2pair_map i2p;

  vector<string> num_cache;
  string delimiter = ".";
  uint_fast32_t next_ix = 1;

  R_xlen_t max_len(const deque<Node>& nodes) {
    R_xlen_t max_len = 1;
    for (const Node& node: nodes) {
      if (Rf_isFrame(node.obj)) {
        max_len *= nrows(CAR(node.obj)); // from nrows
      } else {
        max_len *= XLENGTH(node.obj);
      }
    }
    return max_len;
  }

  R_xlen_t max_len(const deque<Node>& nodes, unordered_set<uint_fast32_t> multitypes) {
    R_xlen_t max_len = 1;
    for (const Node& node: nodes) {
      if (multitypes.find(node.ix) == multitypes.end()) {
        max_len = XLENGTH(node.obj) * max_len;
      }
    }
    return max_len;
  }

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

  inline void add_node(deque<Node>& acc, SEXP x, uint_fast32_t ix) {
    R_xlen_t N = XLENGTH(x);
    bool stack = Rf_getAttrib(x, R_NamesSymbol) == R_NilValue;

    if (N > 0)  {
      if (TYPEOF(x) == VECSXP) {
        if (stack) {
          add_stacked_nodes(acc, x, ix);
        } else {
          add_nodes(acc, x, ix);
        }
      } else {
        acc.push_front(Node(ix, x));
      }
    }
  }

  void add_nodes(deque<Node>& acc, SEXP x, uint_fast32_t parent_ix) {

	R_xlen_t N = XLENGTH(x);

	SEXP names = Rf_getAttrib(x, R_NamesSymbol);
	bool has_names = names != R_NilValue;
	if (!has_names) {
      populate_num_cache(N);
	}

	for (R_xlen_t i = 0; i < N; i++) {

      P("N: i:%ld type:%s\n", i, Rf_type2char(TYPEOF(x)));

      const char* cname =
		has_names ? CHAR(STRING_ELT(names, i)) : num_cache[i].c_str();

	  add_node(acc,
               VECTOR_ELT(x, i),
               child_ix(parent_ix, cname));
	}
  }

  void add_stacked_nodes(deque<Node>& acc, SEXP x, uint_fast32_t parent_ix) {
    R_xlen_t N = XLENGTH(x);

    unordered_map<uint_fast32_t, SEXPTYPE> i2type;
    unordered_set<uint_fast32_t> multitypes;
    vector<deque<Node>> nodes_vec(N);

    for (R_xlen_t i = 0; i < N; i++) {
      P("S: i:%ld type:%s\n", i, Rf_type2char(TYPEOF(x)));

      add_node(nodes_vec[i],
               VECTOR_ELT(x, i),
               parent_ix);

      for (Node& node: nodes_vec[i]) {
        SEXPTYPE node_type = TYPEOF(node.obj);
        auto typeit = i2type.find(node.ix);
        if (typeit == i2type.end()) {
          i2type.insert(make_pair(node.ix, node_type));
        } else {
          if ((typeit->second == VECSXP) &&
              (typeit->second != node_type)) {
            P("mtype: %s!=%s\n", Rf_type2char(typeit->second), Rf_type2char(node_type));
            multitypes.insert(node.ix);
            typeit->second = VECSXP;
          }
        }
      }
    }

    R_xlen_t len = 0;
    vector<R_xlen_t> lens(N);
    for (R_xlen_t i = 0; i < N; i++) {
      lens[i] = max_len(nodes_vec[i], multitypes);
      len += lens[i];
    }

    P("len:%ld\n", len);

    // initialize out-nodes
    unordered_map<uint_fast32_t, Node> out_nodes;

    SEXP out = PROTECT(Rf_allocVector(VECSXP, i2type.size()));
    // FIXME: Invalid data.frame. Differentiate between our and user data.frames.
    Rf_setAttrib(out, R_ClassSymbol, ScalarString(mkChar("data.frame")));

    R_xlen_t i = 0;
    for (auto& pair: i2type) {
      SEXP vec = make_na_vector(pair.second, len);
      SET_VECTOR_ELT(out, i, vec);
      out_nodes.insert(make_pair(pair.first, Node(pair.first, vec)));
    }

    // fill out nodes
    R_xlen_t from = 0;
    for (R_xlen_t i = 0; i < N; i++) {
      P("i:%ld\n", i);
      R_len_t to = from + lens[i];
      for (Node& node: nodes_vec[i]) {
        Node target = out_nodes.find(node.ix)->second;
        if (multitypes.find(node.ix) == multitypes.end()) {
          fill_vector(node.obj, target.obj, from, to);
          Rf_PrintValue(target.obj);
        } else {
          for (R_xlen_t j = from; j < from + lens[i]; j++) {
            SET_VECTOR_ELT(target.obj, j, lazy_duplicate(node.obj));
          }
        }
      }
      from = to;
    }

    UNPROTECT(1);
    acc.push_front(Node(parent_ix, out));

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

	deque<Node> raw_nodes, nodes;
	add_nodes(rawnodes, x, 0);

	R_xlen_t out_len = max_len(nodes);

    for (Node& node: raw_nodes) {
      if (isFrame(node.obj)) {
        // unnest data.frames
        R_xlen_t df_len = XLENGTH(node.obj);
        if (df_len > 0) {
          SEXP df = node.obj;
          SEXP df_names = Rf_getAttrib(df, R_NamesSymbol);
          uint_fast32_t parent_ix = node.ix;
          node.obj = R_NilValue;
          for (R_xlen_t i = 0; i < df_len; i++) {
            Node node = Node(child_ix(parent_ix, CHAR(STRING_ELT(df_names, i))),
                             VECTOR_ELT(df, i));
            nodes.push_front(node);
          }
        } else {
          // remove NULL objects
          if (node.obj != R_NilValue) {
            nodes.push_front(node);
          }
        }
      }
    }

    SEXP out = PROTECT(Rf_allocVector(VECSXP, nodes.size()));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, nodes.size()));

    R_xlen_t i = nodes.size() - 1,

	for (Node& node : nodes) {
      bool is_frame = Rf_isFrame(node.obj);
	  R_xlen_t this_len = is_frame ? XLENGTH(CAR(node.obj)) : XLENGTH(node.obj);
	  SEXP obj;
	  if (this_len == out_len) {
		obj = node.obj;
	  } else {
		obj = rep_vector(node.obj, out_len);
	  }
	  SET_VECTOR_ELT(out, i, obj);
      string name = full_name(node.ix);
	  SET_STRING_ELT(names, i, Rf_mkCharLenCE(name.c_str(), name.size(), CE_UTF8));
	  i--;
	}

	// build data.frame
	SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
	INTEGER(row_names)[0] = NA_INTEGER;
	INTEGER(row_names)[1] = -out_len;
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
