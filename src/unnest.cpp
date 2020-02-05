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

typedef unordered_map<pair<uint_fast32_t, const char*>, uint_fast32_t, hash_pair> cpair2ix_map;
typedef unordered_map<uint_fast32_t, pair<uint_fast32_t, const char*>> ix2cpair_map;

typedef unordered_map<pair<uint_fast32_t, int>, uint_fast32_t, hash_pair> ipair2ix_map;
typedef unordered_map<uint_fast32_t, pair<uint_fast32_t, int>> ix2ipair_map;

struct NodeAccumulator {
  R_xlen_t nrows = 1;
  deque<unique_ptr<Node>> pnodes;
};

// TODO: make this into a constructor?
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
    done_children = false, done_stack = false,
    done_exclude = false;
  SEXP children = R_NilValue;
  Spec spec;

  for (R_xlen_t i = 0; i < N; i++) {
    SEXP obj = VECTOR_ELT(lspec, i);
    if (obj != R_NilValue) {
      const char* nm = CHAR(STRING_ELT(names, i));

      if (!done_node && !strcmp(nm, "node")) {
        if (XLENGTH(obj) != 1)
          Rf_error("spec's 'node' fields must be of length 1");
        switch(TYPEOF(obj)) {
         case STRSXP: spec.node = STRING_ELT(obj, 0); break;
         case INTSXP: spec.ix = INTEGER(obj)[0] - 1; break;
         case REALSXP: spec.ix = REAL(obj)[0] - 1; break;
         default:
          Rf_error("spec's 'node' field must be NULL, string or numeric vector");
        }
        done_node = true;
      } else  if (!done_name && !strcmp(nm, "name")) {
        if (TYPEOF(obj) != STRSXP || XLENGTH(obj) != 1)
          Rf_error("spec's 'name' field must be a string vector of length 1");
        spec.name = STRING_ELT(obj, 0);
        done_name = true;
      } else if (!done_stack && !strcmp(nm, "stack")) {
        if (TYPEOF(obj) != LGLSXP || XLENGTH(obj) != 1)
          Rf_error("spec's 'stack' field must be a logical vector of length 1");
        spec.stack = LOGICAL(obj)[0];
        done_stack = true;
      } else if (!done_children && !strcmp(nm, "children")) {
        if (TYPEOF(obj) != VECSXP)
          Rf_error("spec's 'children' field must be a list");
        children = obj;
        done_children = true;
      } else if (!done_exclude && !strcmp(nm, "exclude")) {
        if (TYPEOF(obj) != STRSXP)
          Rf_error("spec's 'exclude' field must be a character vector");
        spec.exclude = obj;
        done_exclude = true;
      }

    }
  }

  // always has name unless node is NULL
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

  cpair2ix_map cp2i;
  ix2cpair_map i2cp;

  ipair2ix_map ip2i;
  ix2ipair_map i2ip;

  vector<string> num_cache;
  string delimiter = ".";
  uint_fast32_t next_ix = 1;

  /* const char* ix2char(int ix) { */
  /*   if (ix < 0) return ""; */
  /*   if (ix >= num_cache.size()) */
  /*     num_cache.resize(ix + 1, ""); */
  /*   string out = num_cache[ix]; */
  /*   if (out == "") { */
  /*     out = to_string(ix + 1); */
  /*     num_cache[ix] = out; */
  /*   } */
  /*   return out.c_str(); */
  /* } */

  uint_fast32_t child_ix(uint_fast32_t parent_ix, const char* cname) {
    //  pname: aka pair name, <parent ix, char* this name>
    auto pname = make_pair(parent_ix, cname);
    uint_fast32_t ix;
    cpair2ix_map::iterator pnameit = cp2i.find(pname);
    if (pnameit == cp2i.end()) {
      ix = next_ix++;
      cp2i.insert(make_pair(pname, ix));
      i2cp.insert(make_pair(ix, pname));
    } else {
      ix = pnameit->second;
    }
    return ix;
  }

  uint_fast32_t child_ix(uint_fast32_t parent_ix, int cix) {
    auto pix = make_pair(parent_ix, cix);
    uint_fast32_t ix;
    ipair2ix_map::iterator pixit = ip2i.find(pix);
    if (pixit == ip2i.end()) {
      ix = next_ix++;
      ip2i.insert(make_pair(pix, ix));
      i2ip.insert(make_pair(ix, pix));
    } else {
      ix = pixit->second;
    }
    return ix;
  }

  uint_fast32_t child_ix(uint_fast32_t parent_ix, const SpecMatch& match) {
    return (match.name == R_NilValue) ?
      child_ix(parent_ix, match.ix):
      child_ix(parent_ix, CHAR(match.name));
  }

  string full_name(uint_fast32_t ix) {

	if (ix == 0)
	  return "";

	ix2cpair_map::iterator pcit;
    ix2ipair_map::iterator piit;

    forward_list<string> acc;

	do {
      pcit = i2cp.find(ix);
	  if (pcit == i2cp.end()) {
        piit = i2ip.find(ix);
        if (piit == i2ip.end())
          Rf_error("[Bug] Iname not in index hashmaps, please report");
        ix = piit->second.first;
        acc.push_front(to_string(piit->second.second + 1));
      } else {
        ix = pcit->second.first;
        if (*(pcit->second.second) != '\0') {
          acc.emplace_front(pcit->second.second);
        }
      }
    } while (ix != 0);

    if (acc.empty())
      return "";

	string out = acc.front();
	acc.pop_front();

	while (!acc.empty()) {
	  out.append(delimiter).append(acc.front());
	  acc.pop_front();
	}

	return out;
  }

  void add_node(NodeAccumulator& acc, const Spec& spec,
                SEXP x, uint_fast32_t ix) {
    R_xlen_t N = XLENGTH(x);
    if (N > 0) {
      if (TYPEOF(x) == VECSXP) {
        P("--> add node:%s(%ld) %s\n", full_name(ix).c_str(), ix, spec.to_string().c_str());

        SEXP names = Rf_getAttrib(x, R_NamesSymbol);
        bool has_names = names != R_NilValue;

        const vector<SpecMatch>& matches = spec.match(x);
        P("  (matches: %ld)\n", matches.size());
        if (spec.stack) {
          stack_nodes(acc, spec, matches, ix);
        } else {
          for (const SpecMatch& m: matches) {
            uint_fast32_t cix = child_ix(ix, m);
            P("  (cix: %ld match: %s)\n", cix, m.to_string().c_str());
            if (spec.children.empty()) {
              add_node(acc, NilSpec, m.obj, cix);
            } else {
              for (const Spec& cspec: spec.children) {
                add_node(acc, cspec, m.obj, cix);
              }
            }
          }
        }
        P("<-- added node:%s(%ld) acc[%ld,%ld]\n",
          full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
      } else {
        if (spec.children.size() == 0) {
          acc.pnodes.push_front(make_unique<SexpNode>(ix, x));
          acc.nrows *= N;
          P("-- added sexp node:%s(%ld) acc[%ld,%ld]\n",
            full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
        }
      }
    }
  }

  void stack_nodes(NodeAccumulator& acc,
                   const Spec& spec,
                   const vector<SpecMatch>& matches,
                   uint_fast32_t ix) {
    P(">>> stack_nodes ---\n");
    size_t N = matches.size();
    R_xlen_t beg = 0, end=0;
    unordered_map<uint_fast32_t, unique_ptr<RangeNode>> out_nodes;

    for (const SpecMatch& m: matches) {
      NodeAccumulator iacc;

      // TODO: keep index/names in a key column
      /* uint_fast32_t cix = (m.name == R_NilValue) ? ix : child_ix(ix, CHAR(m.name)); */
      if (spec.children.empty()) {
        add_node(iacc, NilSpec, m.obj, ix);
      } else {
        for (const Spec& cspec: spec.children) {
          add_node(iacc, cspec, m.obj, ix);
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
    P("<<< stack_nodes ---\n");

  }


 public:

  SEXP process(SEXP x, SEXP lspec) {
    const Spec spec = (lspec == R_NilValue) ? NilSpec : list2spec(lspec);

	NodeAccumulator acc;
	add_node(acc, spec, x, 0);

    size_t ncols = acc.pnodes.size();
    size_t nrows = (ncols > 0 ? acc.nrows : 0);

    P("FINAL: nrow:%ld ncol:%ld\n", nrows, ncols);
    // temporary output holder to avoid protecting each element
    SEXP tout = PROTECT(Rf_allocVector(VECSXP, ncols));
    vector<string> str_names;
    str_names.reserve(ncols);

    R_xlen_t i = 0;

	for (unique_ptr<Node>& p: acc.pnodes) {
      P("alloc type: %s\n", Rf_type2char(p->type()));
      if (p->type() == VECSXP)
        Rf_error("Cannot handle irregular types yet");
      SEXP obj = make_na_vector(p->type(), nrows);
	  SET_VECTOR_ELT(tout, i, obj);
      p->copy_into(obj, 0, nrows);
      str_names.push_back(full_name(p->ix));
      P("ADDED:%s(%ld)\n", full_name(p->ix).c_str(), p->ix);
	  i++;
	}

    SEXP names = PROTECT(Rf_allocVector(STRSXP, ncols));
    SEXP out = PROTECT(Rf_allocVector(VECSXP, ncols));

    vector<size_t> ixes = orderix(str_names);

    for (size_t i: ixes) {
      const string& nm = str_names[ixes[i]];
	  SET_STRING_ELT(names, i, Rf_mkCharLenCE(nm.c_str(), nm.size(), CE_UTF8));
      SET_VECTOR_ELT(out, i, VECTOR_ELT(tout, ixes[i]));
    }

	// build data.frame
	SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
	INTEGER(row_names)[0] = NA_INTEGER;
	INTEGER(row_names)[1] = nrows;
	Rf_setAttrib(out, R_ClassSymbol, ScalarString(mkChar("data.frame")));
	Rf_setAttrib(out, R_RowNamesSymbol, row_names);
	Rf_setAttrib(out, R_NamesSymbol, names);
	UNPROTECT(4);

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
