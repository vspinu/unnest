#ifndef UNNEST_UNNESTER_H
#define UNNEST_UNNESTER_H

#include <deque>
#include <algorithm>
#include <utility>
#include <forward_list>
#include <unordered_map>
#include <unordered_set>
#include "common.h"
#include "Node.h"
#include "VarAccumulator.h"
#include "Spec.h"

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


// cannot define templates in non-templates class, so have to do this extraction
template<class UT, class AT>
void add_node(UT& U, AT& acc, VarAccumulator& vacc,
              const Spec& spec, uint_fast32_t ix, SEXP x) {
  if (x == R_NilValue || XLENGTH(x) == 0) return; // XLENGTH doesn't work on NULL
  if (&spec == &NilSpec && vacc.has_var(ix)) return;
  P("nill,leaf spec: [%d,%d]\n", &spec == &NilSpec, &spec == &LeafSpec);
  if (spec.dedupe == Spec::Dedupe::INHERIT) {
    U.add_node_impl(acc, vacc, spec, ix, x);
  } else {
    VarAccumulator nvacc(spec.dedupe == Spec::Dedupe::TRUE);
    U.add_node_impl(acc, nvacc, spec, ix, x);
  }
}

struct Unnester {

  cpair2ix_map cp2i;
  ix2cpair_map i2cp;

  ipair2ix_map ip2i;
  ix2ipair_map i2ip;

  vector<string> num_cache;
  string delimiter = ".";
  uint_fast32_t next_ix = 1;

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

  void add_node_impl(NodeAccumulator& acc, VarAccumulator& vacc,
                     const Spec& spec, uint_fast32_t ix, SEXP x) {
    if (TYPEOF(x) == VECSXP) {
      P("--> add node:%s(%ld) %s\n", full_name(ix).c_str(), ix, spec.to_string().c_str());
      const vector<SpecMatch>& matches = spec.match(x);
      P("  (matches: %ld)\n", matches.size());
      if (spec.stack) {
        stack_nodes(acc, vacc, spec, ix, matches);
      } else {
        spread_nodes(acc, vacc, spec, ix, matches);
      }
      P("<-- added node:%s(%ld) acc[%ld,%ld]\n",
        full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
    } else {
      if (&spec == &LeafSpec || &spec == &NilSpec) {
        acc.pnodes.push_front(make_unique<SexpNode>(ix, x));
        acc.nrows *= XLENGTH(x);
        P("-- added sexp node:%s(%ld) acc[%ld,%ld]\n",
          full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
      }
    }
  }

  void add_node_impl(vector<NodeAccumulator>& accs, VarAccumulator& vacc,
                     const Spec& spec, uint_fast32_t ix, SEXP x) {
    if (TYPEOF(x) == VECSXP) {
      if (spec.stack) {
        const vector<SpecMatch>& matches = spec.match(x);
        stack_nodes(accs, vacc, spec, 0, matches);
      } else {
        Rf_error("Grouped spreading is not yet implemented");
      }
    } else if (spec.children.size() > 0) {
      // fixme: not entirely sure what should be the logic here
      Rf_error("Cannot use grouped unnesting on a non-list");
    }
  }

  void spread_nodes(NodeAccumulator& acc, VarAccumulator& vacc,
                    const Spec& spec, uint_fast32_t ix,
                    const vector<SpecMatch>& matches) {
    for (const SpecMatch& m: matches) {
      uint_fast32_t cix = child_ix(ix, m);
      dispatch_match_to_child(acc, vacc, spec, cix, m);
    }
  }

  inline void dispatch_match_to_child(NodeAccumulator& acc, VarAccumulator& vacc,
                                      const Spec& spec, uint_fast32_t cix,
                                      const SpecMatch& m) {
    P("  (cix: %ld match: %s)\n", cix, m.to_string().c_str());
    if (&spec == &NilSpec || &spec == &LeafSpec) {
      add_node(*this, acc, vacc, NilSpec, cix, m.obj);
      vacc.add_var(cix);
    } else if (spec.children.empty()) {
      add_node(*this, acc, vacc, LeafSpec, cix, m.obj);
      vacc.add_var(cix);
    } else {
      for (const Spec& cspec: spec.children) {
        add_node(*this, acc, vacc, cspec, cix, m.obj);
      }
    }
  }

  void stack_nodes(NodeAccumulator&, VarAccumulator& vacc,
                   const Spec& spec, uint_fast32_t ix,
                   const vector<SpecMatch>& matches);

  void stack_nodes(vector<NodeAccumulator>&, VarAccumulator& vacc,
                   const Spec& spec, uint_fast32_t ix,
                   const vector<SpecMatch>& matches);

 public:

  SEXP process(SEXP x, SEXP lspec) {
    const Spec spec = (lspec == R_NilValue) ? NilSpec : list2spec(lspec);

    size_t Ngr = spec.groups.size();

    VarAccumulator vacc(false);

    if (Ngr == 0) {
      NodeAccumulator acc;
      add_node(*this, acc, vacc, spec, 0, x);
      return build_df(acc, vacc);
    } else {
      SEXP names = PROTECT(Rf_allocVector(STRSXP, Ngr));
      SEXP out = PROTECT(Rf_allocVector(VECSXP, Ngr));
      vector<NodeAccumulator> accs(Ngr);

      add_node(*this, accs, vacc, spec, 0, x);

      for (size_t gi = 0; gi < Ngr; gi++) {
        SET_STRING_ELT(names, gi, get<0>(spec.groups[gi]));
        SET_VECTOR_ELT(out, gi, build_df(accs[gi], vacc));
      }

      Rf_setAttrib(out, R_NamesSymbol, names);
      UNPROTECT(2);
      return out;
    }
  }

  SEXP build_df(NodeAccumulator& acc, VarAccumulator& vacc) {

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

#endif
