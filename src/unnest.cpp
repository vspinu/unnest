#include <deque>
#include <algorithm>
#include <utility>
#include <forward_list>
#include <unordered_map>
#include <unordered_set>
#include "Node.h"
#include "common.h"

#undef nrows

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

struct NodeAccumulator {
  R_xlen_t nrows = 1;
  deque<unique_ptr<Node>> pnodes;
};

Spec list2spec(SEXP lspec) {
  if (TYPEOF(lspec) != VECSXP)
    Rf_error("'spec' must be a list");
  if (!isSpec(lspec))
    Rf_error("'spec' must be of S3 class 'unnest.spec'");

  SEXP names = Rf_getAttrib(lspec, R_NamesSymbol);
  if (names == R_NilValue)
    Rf_error("unnest.spec must have non-nil names");

  R_xlen_t N = LENGTH(lspec);
  bool
    done_node = false, done_name = false,
    done_children = false, done_stack = false;
  SEXP children = R_NilValue;
  Spec spec;
  spec.name = R_NilValue;
  spec.node = R_NilValue;

  for (R_xlen_t i = 0; i < N; i++) {
    SEXP obj = VECTOR_ELT(lspec, i);
    if (obj != R_NilValue) {
      const char* nm = CHAR(STRING_ELT(names, i));
      if (!done_node && !strcmp(nm, "node")) {
        if (TYPEOF(obj) != STRSXP || XLENGTH(obj) != 1)
          Rf_error("spec 'node' field must be a string vector of length 1");
        spec.node = STRING_ELT(obj, 0);
        done_node = true;
      } else  if (!done_name && !strcmp(nm, "name")) {
        if (TYPEOF(obj) != STRSXP || XLENGTH(obj) != 1)
          Rf_error("spec 'name' field must be a string vector of length 1");
        spec.name = STRING_ELT(obj, 0);
        done_name = true;
      } else if (!done_stack && !strcmp(nm, "stack")) {
        if (TYPEOF(obj) != LGLSXP || XLENGTH(obj) != 1)
          Rf_error("spec 'stack' field must be a logical vector of length 1");
        spec.stack = LOGICAL(obj)[0];
        done_stack = true;
      } else if (!done_children && !strcmp(nm, "children")) {
        if (TYPEOF(obj) != VECSXP)
          Rf_error("spec 'children' field must be a list");
        children = obj;
        done_children = true;
      }
    }
  }

  // always have name unless node is NULL
  if (spec.node != R_NilValue && spec.name == R_NilValue)
    spec.name = spec.node;

  if (children != R_NilValue) {
    R_xlen_t NC = XLENGTH(children);
    spec.children.reserve(NC);
    for (R_xlen_t c = 0; c < NC; c++) {
      SEXP ch = VECTOR_ELT(children, c);
      spec.children.emplace_back(list2spec(ch));
    }
  }
  return spec;
}

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

  inline void add_node(NodeAccumulator& acc, const Spec& spec,
                       SEXP x, uint_fast32_t ix) {
    R_xlen_t N = XLENGTH(x);
    if (N > 0) {
      if (TYPEOF(x) == VECSXP) {
        if (spec.stack) {
          add_stacked_nodes(acc, spec, x, ix);
          P("added stacked node:%s(%ld) acc[%ld,%ld]\n",
            full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
        } else {
          add_vec_nodes(acc, spec, x, ix);
          P("added vec node:%s(%ld) acc[%ld,%ld]\n",
            full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
        }
      } else {
        acc.pnodes.push_front(make_unique<SexpNode>(ix, x));
        acc.nrows *= N;
        P("added sexp node:%s(%ld) acc[%ld,%ld]\n",
          full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
      }
    }
  }

  void add_vec_nodes(NodeAccumulator& acc, const Spec& spec,
                     SEXP x, uint_fast32_t ix) {
    R_xlen_t N = XLENGTH(x);
    SEXP names = Rf_getAttrib(x, R_NamesSymbol);
    bool has_names = names != R_NilValue;
    if (!has_names) {
      populate_num_cache(N);
    }

    if (&spec == &NilSpec || spec.children.size() == 0) {
      for (R_xlen_t i = 0; i < N; i++) {
        const char* cname =
          has_names ? CHAR(STRING_ELT(names, i)) : num_cache[i].c_str();
        add_node(acc,
                 NilSpec,
                 VECTOR_ELT(x, i),
                 child_ix(ix, cname));
      }
    } else {
      for (R_xlen_t i = 0; i < N; i++) {
        SEXP nm = STRING_ELT(names, i);
        for (const auto& pspec: spec.children) {
          if (pspec.node == R_NilValue || pspec.node == nm) {
            const char* cname;
            if (pspec.name != R_NilValue) {
              cname = CHAR(pspec.name);
            } else {
              cname = has_names ? CHAR(STRING_ELT(names, i)) : num_cache[i].c_str();
            }
            add_node(acc,
                     pspec,
                     VECTOR_ELT(x, i),
                     child_ix(ix, cname));
          }
        }
      }
    }
  }

  void add_stacked_nodes(NodeAccumulator& acc, const Spec& spec,
                         SEXP x, uint_fast32_t ix) {
    R_xlen_t N = XLENGTH(x);
    R_xlen_t beg = 0, end=0;
    unordered_map<uint_fast32_t, unique_ptr<RangeNode>> out_nodes;

    SEXP names = Rf_getAttrib(x, R_NamesSymbol);
    bool has_names = names != R_NilValue;

    for (R_xlen_t i = 0; i < N; i++) {
      NodeAccumulator iacc;
      if (pspec.children.size() == 0) {
        add_node(iacc, NilSpec, VECTOR_ELT(x, i), ix);
      } else {
        for (const Spec& cspec: pspec.children) {
          P("--- here:\n");
          add_node(iacc, cspec, VECTOR_ELT(x, i), ix);
          /* if (cspec.node == R_NilValue || */
          /*     (has_names && cspec.node == STRING_ELT(names, i))) { */
          /*   add_node(iacc, pspec, VECTOR_ELT(x, i), ix); */
          /* } */
        }
      }
      end += iacc.nrows;

      // move to out_nodes
      while (!iacc.pnodes.empty()) {
        unique_ptr<Node>& ip = iacc.pnodes.front();
        auto oit = out_nodes.find(ip->ix);
        if (oit == out_nodes.end()) {
          unique_ptr<RangeNode> pr = make_unique<RangeNode>(ip->ix);
          pr->push(beg, end, move(ip));
          P("stacking new node:%s type:%s range:%ld-%ld Nnodes:%ld\n",
            full_name(pr->ix).c_str(), Rf_type2char(pr->type()),
            beg, end, pr->pnodes.size());
          out_nodes.emplace(pr->ix, move(pr));
        } else {
          P("stacking old node:%s type:%s range:%ld-%ld Nnodes:%ld\n",
            full_name(ip->ix).c_str(), Rf_type2char(ip->type()),
            beg, end, oit->second->pnodes.size()+1);
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
  }


 public:

  SEXP process(SEXP x, SEXP lspec) {
    const Spec spec = (lspec == R_NilValue) ? NilSpec : list2spec(lspec);

	NodeAccumulator acc;
	add_node(acc, spec, x, 0);

    size_t ncols = acc.pnodes.size();
    size_t nrows = (ncols > 0 ? acc.nrows : 0);

    P("FINAL: nrow:%ld ncol:%ld\n", nrows, ncols);
    SEXP out = PROTECT(Rf_allocVector(VECSXP, ncols));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, ncols));

    R_xlen_t i = ncols - 1;

	for (unique_ptr<Node>& p: acc.pnodes) {
      P("alloc type: %s\n", Rf_type2char(p->type()));
      if (p->type() == VECSXP)
        Rf_error("Cannot handle irregular types yet");
      SEXP obj = make_na_vector(p->type(), nrows);
	  SET_VECTOR_ELT(out, i, obj);
      p->copy_into(obj, 0, nrows);
      string name = full_name(p->ix);
	  SET_STRING_ELT(names, i, Rf_mkCharLenCE(name.c_str(), name.size(), CE_UTF8));
	  i--;
	}

	// build data.frame
	SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
	INTEGER(row_names)[0] = NA_INTEGER;
	INTEGER(row_names)[1] = nrows;
	Rf_setAttrib(out, R_ClassSymbol, ScalarString(mkChar("data.frame")));
	Rf_setAttrib(out, R_RowNamesSymbol, row_names);
	Rf_setAttrib(out, R_NamesSymbol, names);
	UNPROTECT(3);

	return out;
  }
};

extern "C" SEXP C_unnest(SEXP x, SEXP lspec) {
  SEXPTYPE type = TYPEOF(x);
  if (TYPEOF(x) != VECSXP) {
	Rf_error("x must be a list vector");
  }

  Unnester unnester;

  return unnester.process(x, lspec);
}
