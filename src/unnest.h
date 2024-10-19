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
  size_t operator()(const std::pair<T1, T2>& p) const
  {
	auto hash1 = std::hash<T1>{}(p.first);
	auto hash2 = std::hash<T2>{}(p.second);
	return hash1 ^ hash2;
  }
};

typedef std::unordered_map<std::pair<uint_fast32_t, const char*>, uint_fast32_t, hash_pair> cpair2ix_map;
typedef std::unordered_map<uint_fast32_t, std::pair<uint_fast32_t, const char*>> ix2cpair_map;

typedef std::unordered_map<std::pair<uint_fast32_t, int>, uint_fast32_t, hash_pair> ipair2ix_map;
typedef std::unordered_map<uint_fast32_t, std::pair<uint_fast32_t, int>> ix2ipair_map;

struct NodeAccumulator {
  R_xlen_t nrows = 1;
  std::deque<std::unique_ptr<Node>> pnodes;
};


// Need to extract this method as C++ does not allows templates in a
// non-templated class.
template<class UT, class AT>
void add_node(UT& U, AT& acc, VarAccumulator& vacc,
              const Spec& pspec, const Spec& spec,
              uint_fast32_t ix, SEXP x,
              bool stack_atomic = false) {
  // XLENGTH doesn't work on NULL
  if (x == R_NilValue || XLENGTH(x) == 0) {
    return;
  }
  P("-> add_node(%ld); vacc: %p; has_var: %d %s\n", ix, &vacc, vacc.has_var(ix), spec.to_string().c_str());
  if (pspec.terminal && vacc.has_var(ix)) {
    P("   already has var %ld\n", ix);
    return;
  }
  U.add_node_impl(acc, vacc, pspec, spec, ix, x, stack_atomic);
  if (spec.terminal) {
    vacc.insert(ix);
  }
}

struct Unnester {

  enum ProcessUnnamed {NONE, STACK, EXCLUDE, ASIS, PASTE};
  ProcessUnnamed sexp2unnamed(SEXP x);


  bool dedupe;
  bool stack_atomic;
  bool stack_atomic_df;
  Spec::Process process_atomic;
  ProcessUnnamed process_unnamed_list;
  bool rep_to_max;

  cpair2ix_map cp2i;
  ix2cpair_map i2cp;

  ipair2ix_map ip2i;
  ix2ipair_map i2ip;

  std::vector<std::string> num_cache;
  std::string delimiter = ".";
  uint_fast32_t next_ix = 1;

  uint_fast32_t child_ix(uint_fast32_t parent_ix, const char* cname) {
    //  pname: aka pair name, <parent ix, char* this name>
    auto pname = std::make_pair(parent_ix, cname);
    uint_fast32_t ix;
    cpair2ix_map::iterator pnameit = cp2i.find(pname);
    if (pnameit == cp2i.end()) {
      ix = next_ix++;
      cp2i.insert(std::make_pair(pname, ix));
      i2cp.insert(std::make_pair(ix, pname));
    } else {
      ix = pnameit->second;
    }
    return ix;
  }

  uint_fast32_t child_ix(uint_fast32_t parent_ix, int cix) {
    auto pix = std::make_pair(parent_ix, cix);
    uint_fast32_t ix;
    ipair2ix_map::iterator pixit = ip2i.find(pix);
    if (pixit == ip2i.end()) {
      ix = next_ix++;
      ip2i.insert(std::make_pair(pix, ix));
      i2ip.insert(std::make_pair(ix, pix));
    } else {
      ix = pixit->second;
    }
    return ix;
  }

  uint_fast32_t child_ix(uint_fast32_t parent_ix, const SpecMatch& match) {
    if (match.spec_name != R_NilValue)
      return child_ix(parent_ix, CHAR(match.spec_name));
    else if (match.elem_name != R_NilValue)
      return child_ix(parent_ix, CHAR(match.elem_name));
    else
      return child_ix(parent_ix, match.ix);
  }

  uint_fast32_t child_ix(uint_fast32_t parent_ix, SEXP cname) {
    return (cname == R_NilValue) ?
      child_ix(parent_ix, CHAR(R_BlankString)):
      child_ix(parent_ix, CHAR(cname));
  }

  std::string full_name(uint_fast32_t ix) {

	if (ix == 0)
	  return "";

	ix2cpair_map::iterator pcit;
    ix2ipair_map::iterator piit;

    std::forward_list<std::string> acc;

	do {
      pcit = i2cp.find(ix);
	  if (pcit == i2cp.end()) {
        piit = i2ip.find(ix);
        if (piit == i2ip.end())
          Rf_error("[Bug] Iname not in index hashmaps, please report");
        ix = piit->second.first;
        acc.push_front(std::to_string(piit->second.second + 1));
      } else {
        ix = pcit->second.first;
        if (*(pcit->second.second) != '\0') {
          acc.emplace_front(pcit->second.second);
        }
      }
    } while (ix != 0);

    if (acc.empty())
      return "";

	std::string out = acc.front();
	acc.pop_front();

	while (!acc.empty()) {
	  out.append(delimiter).append(acc.front());
	  acc.pop_front();
	}

	return out;
  }

  void add_node_impl(NodeAccumulator& acc, VarAccumulator& vacc,
                     const Spec& pspec, const Spec& spec,
                     uint_fast32_t ix, SEXP x, bool stack_atomic) {
    // LENGTH(X) > 0 and X != NULL in here
    if (pspec.process == Spec::Process::ASIS) {
      acc.pnodes.push_front(std::make_unique<AsIsNode>(ix, x));
      P("<--- added ASIS node impl:%s(%ld) acc[%ld,%ld]\n", full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
    } else if (pspec.process == Spec::Process::PASTE) {
      acc.pnodes.push_front(std::make_unique<PasteNode>(ix, x));
      P("<--- added PASTE node impl:%s(%ld) acc[%ld,%ld]\n", full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
    } else if (TYPEOF(x) == VECSXP) {
      // Lists
      bool is_unnamed = Rf_getAttrib(x, R_NamesSymbol) == R_NilValue;
      if (is_unnamed && pspec.terminal && spec.stack == Spec::Stack::AUTO) {
        /* PP("pspec: %s\n", pspec.to_string().c_str()); */
        /* PP("spec: %s\n", spec.to_string().c_str()); */
        if (this->process_unnamed_list == ProcessUnnamed::EXCLUDE) {
          P("<--- excluded UNNAMED node impl:%s(%ld) acc[%ld,%ld]\n", full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
          return;
        } else if (this->process_unnamed_list == ProcessUnnamed::ASIS) {
          acc.pnodes.push_front(std::make_unique<AsIsNode>(ix, x));
          P("<--- added UNNAMED-ASIS node impl:%s(%ld) acc[%ld,%ld]\n", full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
          return;
        } else if (this->process_unnamed_list == ProcessUnnamed::PASTE) {
          acc.pnodes.push_front(std::make_unique<PasteNode>(ix, x));
          P("<--- added UNNAMED-PASTE node impl:%s(%ld) acc[%ld,%ld]\n", full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
          return;
        }
      }
      stack_atomic = stack_atomic || (this->stack_atomic_df && is_data_frame(x));
      P("--> add_node_impl:%s(%ld) %s\n", full_name(ix).c_str(), ix, spec.to_string().c_str());
      const std::vector<SpecMatch>& matches = spec.match(x);
      P("    nr. matches: %ld, stack_atomic: %d\n", matches.size(), stack_atomic);
      if (spec.stack == Spec::Stack::STACK ||
          (is_unnamed && this->process_unnamed_list == ProcessUnnamed::STACK)) {
        stack_nodes(acc, vacc, spec, ix, matches, stack_atomic);
      } else {
        spread_nodes(acc, vacc, spec, ix, matches, stack_atomic);
      }
      P("<-- added node impl:%s(%ld) acc[%ld,%ld]\n",
        full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
    } else {
      // Atomics
      if (spec.terminal) {// Specs deeper than the object itself are ignored
        P("---> add atomic node impl:%s(%ld) %s\n",
          full_name(ix).c_str(), ix, spec.to_string().c_str());
        R_xlen_t N = XLENGTH(x);
        if (spec.process != Spec::Process::NONE &&
            (spec.process != Spec::Process::PASTE_STRING ||
             TYPEOF(x) == STRSXP)) {
          if (spec.process == Spec::Process::ASIS) {
            acc.pnodes.push_front(std::make_unique<AsIsNode>(ix, x));
            P("<--- added ASIS atomic node impl:%s(%ld) acc[%ld,%ld]\n", full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
          } else if (spec.process == Spec::Process::PASTE ||
                     spec.process == Spec::Process::PASTE_STRING) {
            acc.pnodes.push_front(std::make_unique<PasteNode>(ix, x));
            P("<--- added PASTE atomic node impl:%s(%ld) acc[%ld,%ld]\n", full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
          }
        } else if (this->process_atomic != Spec::Process::NONE &&
                   (this->process_atomic != Spec::Process::PASTE_STRING ||
                    TYPEOF(x) == STRSXP)) {
          P("this->process_atomic: %s\n", spec.process_names.at(this->process_atomic).c_str());
          if (this->process_atomic == Spec::Process::ASIS) {
            acc.pnodes.push_front(std::make_unique<AsIsNode>(ix, x));
            P("<--- added ASIS atomic node impl:%s(%ld) acc[%ld,%ld]\n", full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
          } else if (this->process_atomic == Spec::Process::PASTE ||
                     this->process_atomic == Spec::Process::PASTE_STRING) {
            acc.pnodes.push_front(std::make_unique<PasteNode>(ix, x));
            P("<--- added PASTE atomic node impl:%s(%ld) acc[%ld,%ld]\n", full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
          }
        } else if (spec.stack == Spec::Stack::STACK ||
                   (spec.stack == Spec::Stack::AUTO &&
                    (stack_atomic || this->stack_atomic))) {
          acc.pnodes.push_front(std::make_unique<SexpNode>(ix, x));
          if (stack_atomic || this->rep_to_max)
            acc.nrows = std::max(acc.nrows, N);
          else
            acc.nrows *= N;
          P("<--- added stacked atomic node impl:%s(%ld) acc[%ld,%ld]\n",
            full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
        } else {
          // current implementation doesn't allow spec at a sub-vector level
          if (N == 1) {
            acc.pnodes.push_front(std::make_unique<SexpNode>(ix, x));
          } else {
            acc.pnodes.push_front(std::make_unique<ElNode>(ix, 0, x));
            for (R_xlen_t i = 1; i < N; i++) {
              acc.pnodes.push_front(std::make_unique<ElNode>(child_ix(ix, i), i, x));
            }
          }
          P("<--- added spreaded atomic node impl:%s(%ld) acc[%ld,%ld]\n",
            full_name(ix).c_str(), ix, acc.nrows, acc.pnodes.size());
        }
      }
    }
  }

  void add_node_impl(std::vector<NodeAccumulator>& accs, VarAccumulator& vacc,
                     const Spec& /*unused*/, const Spec& spec,
                     uint_fast32_t /*unused*/, SEXP x, bool stack_atomic = false) {
    if (TYPEOF(x) == VECSXP) {
      if (spec.stack == Spec::Stack::STACK) {
        const std::vector<SpecMatch>& matches = spec.match(x);
        stack_nodes(accs, vacc, spec, 0, matches,
                    stack_atomic || (this->stack_atomic_df && is_data_frame(x)));
      } else {
        Rf_error("Grouped spreading is not yet implemented");
      }
    } else if (spec.children.size() > 0) {
      // fixme: not entirely sure what should be the logic here
      Rf_error("Cannot use grouped unnesting on a non-list");
    }
  }

  inline void dispatch_match_to_child(NodeAccumulator& acc, VarAccumulator& vacc,
                                      const Spec& spec,
                                      uint_fast32_t cix, const SpecMatch& m,
                                      bool stack_atomic = false) {
    P("---> dispatching cix:%ld %s %s\n", cix, m.to_string().c_str(), spec.to_string().c_str());
    if (spec.children.empty()) {
      add_node(*this, acc, vacc, spec, NilSpec, cix, m.obj, stack_atomic);
    } else {
      for (const Spec& cspec: spec.children) {
        add_node(*this, acc, vacc, spec, cspec, cix, m.obj, stack_atomic);
      }
    }
  }

  inline void spread_nodes(NodeAccumulator& acc, VarAccumulator& vacc,
                           const Spec& spec, uint_fast32_t ix,
                           const std::vector<SpecMatch>& matches,
                           bool stack_atomic) {
    for (const SpecMatch& m: matches) {
      uint_fast32_t cix = child_ix(ix, m);
      dispatch_match_to_child(acc, vacc, spec, cix, m, stack_atomic);
    }
  }

  void stack_nodes(NodeAccumulator&, VarAccumulator& vacc, const Spec& spec,
                   uint_fast32_t ix, const std::vector<SpecMatch>& matches,
                   const bool rep_to_max);

  void stack_nodes(std::vector<NodeAccumulator>&, VarAccumulator& vacc, const Spec& spec,
                   uint_fast32_t ix, const std::vector<SpecMatch>& matches,
                   const bool rep_to_max);

 public:

  SEXP process(SEXP x, SEXP lspec) {
    const Spec spec = (lspec == R_NilValue) ? NilSpec : sexp2spec(lspec);

    size_t Ngr = spec.groups.size();

    VarAccumulator vacc(this->dedupe);

    if (Ngr == 0) {
      NodeAccumulator acc;
      add_node(*this, acc, vacc, NilSpec, spec, 0, x);
      SEXP out = build_df(acc);
      return out;
    } else {
      SEXP names = PROTECT(Rf_allocVector(STRSXP, Ngr));
      SEXP out = PROTECT(Rf_allocVector(VECSXP, Ngr));
      std::vector<NodeAccumulator> accs(Ngr);

      add_node(*this, accs, vacc, NilSpec, spec, 0, x);

      for (size_t gi = 0; gi < Ngr; gi++) {
        SET_STRING_ELT(names, gi, std::get<0>(spec.groups[gi]));
        SET_VECTOR_ELT(out, gi, build_df(accs[gi]));
      }

      Rf_setAttrib(out, R_NamesSymbol, names);
      UNPROTECT(2);
      return out;
    }
  }

  SEXP build_df(NodeAccumulator& acc) {

    size_t ncols = acc.pnodes.size();
    size_t nrows = (ncols > 0 ? acc.nrows : 0);
    if (acc.nrows < 0) {
      Rf_error("Output exceeds 64bit vector length. Wrong spec, or you want 'cross_join = FALSE'?");
    }

    P("FINAL: nrow:%ld ncol:%ld\n", nrows, ncols);
    // temporary output holder to avoid protecting each element
    SEXP tout = PROTECT(Rf_allocVector(VECSXP, ncols));
    std::vector<std::string> str_names;
    str_names.reserve(ncols);

    R_xlen_t i = 0;

	for (std::unique_ptr<Node>& p: acc.pnodes) {
      P("alloc type: %s\n", Rf_type2char(p->type()));
      SEXP obj = make_na_vector(p->type(), nrows);
	  SET_VECTOR_ELT(tout, i, obj);
      p->copy_into(obj, 0, nrows);
      str_names.push_back(full_name(p->ix));
      P("ADDED:%s(%ld)\n", full_name(p->ix).c_str(), p->ix);
	  i++;
	}

    SEXP names = PROTECT(Rf_allocVector(STRSXP, ncols));
    SEXP out = PROTECT(Rf_allocVector(VECSXP, ncols));

    std::vector<size_t> ixes = orderix(str_names);

    for (size_t i: ixes) {
      const std::string& nm = str_names[ixes[i]];
	  SET_STRING_ELT(names, i, Rf_mkCharLenCE(nm.c_str(), nm.size(), CE_UTF8));
      SET_VECTOR_ELT(out, i, VECTOR_ELT(tout, ixes[i]));
    }

	// build data.frame
	SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
	INTEGER(row_names)[0] = NA_INTEGER;
	INTEGER(row_names)[1] = nrows;
	Rf_setAttrib(out, R_ClassSymbol, Rf_ScalarString(Rf_mkChar("data.frame")));
	Rf_setAttrib(out, R_RowNamesSymbol, row_names);
	Rf_setAttrib(out, R_NamesSymbol, names);
	UNPROTECT(4);

	return out;

  }
};

#endif
