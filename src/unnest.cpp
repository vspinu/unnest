#include "unnest.h"

Unnester::ProcessUnnamed sexp2unnamed(SEXP x) {
  if (x == R_NilValue)
    return Unnester::ProcessUnnamed::NONE;
  if (TYPEOF(x) == STRSXP) {
    const char* nm = CHAR(STRING_ELT(x, 0));
    if (!strcmp(nm, "stack"))
      return Unnester::ProcessUnnamed::STACK;
    else if (!strcmp(nm, "exclude"))
      return Unnester::ProcessUnnamed::EXCLUDE;
    else if (!strcmp(nm, "as_is") || !strcmp(nm, "as.is") || !strcmp(nm, "asis"))
      return Unnester::ProcessUnnamed::ASIS;
    else if (!strcmp(nm, "paste"))
      return Unnester::ProcessUnnamed::PASTE;
  }
  Rf_error("Invalid `unnamed_list` argument. Must be one of 'as_is', 'exclude', 'stack', 'paste' or NULL");
}

// simple stacker
void Unnester::stack_nodes(NodeAccumulator& acc, VarAccumulator& vacc,
                           const Spec& pspec, const Spec& spec,
                           uint_fast32_t ix, const vector<SpecMatch>& matches,
                           const bool stack_atomic = false) {
  P(">>> stack_nodes ---\n");
  R_xlen_t beg = 0, end=0;
  unordered_map<uint_fast32_t, unique_ptr<RangeNode>> out_nodes;

  bool do_ix = spec.ix_name != R_NilValue;
  unique_ptr<IxNode> pix;
  if (do_ix)
    pix = make_unique<IxNode>(child_ix(ix, CHAR(spec.ix_name)));

  uint_fast32_t cix = child_ix(ix, spec.name);

  for (const SpecMatch& m: matches) {
    NodeAccumulator iacc;
    VarAccumulator ivacc(vacc.dedupe);

    dispatch_match_to_child(iacc, ivacc, pspec, spec, cix, m, stack_atomic);
    end += iacc.nrows;

    // add index
    if (do_ix)
      pix->push(beg, end, m.ix + 1, m.elem_name);

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

  if (do_ix) {
    pix->set_size(end);
    acc.pnodes.push_front(move(pix));
  }

  for (auto& onode: out_nodes) {
    onode.second->set_size(end);
    P("stacked node:%s type:%s, size:%ld\n",
      full_name(onode.second->ix).c_str(), Rf_type2char(onode.second->type()), onode.second->size());
    acc.pnodes.push_front(move(onode.second));
  }

  if (stack_atomic || this->rep_to_max)
    acc.nrows = max(acc.nrows, end);
  else
    acc.nrows = acc.nrows * end;

  P("<<< stack_nodes ---\n");

}

// grouped stacker
void Unnester::stack_nodes(vector<NodeAccumulator>& accs, VarAccumulator& vacc,
                           const Spec& pspec, const Spec& spec,
                           uint_fast32_t ix, const vector<SpecMatch>& matches,
                           const bool stack_atomic = false) {

  if (accs.size() == 0) return;

  P(">>> gstack_nodes ---\n");

  size_t Ngr = spec.groups.size();
  if (accs.size() != Ngr)
    Rf_error("Internal: Invalid grouped stack. Accumulator size (%ld) and spec size (%l) mismatch.",
             accs.size(), Ngr);
  bool do_ix = spec.ix_name != R_NilValue;

  if (spec.children.size() > 0)
    Rf_error("Supplying both children and groups is not yet supported");

  vector<unique_ptr<IxNode>> pixs;
  if (do_ix) {
    uint_fast32_t cix = child_ix(ix, CHAR(spec.ix_name));
    for (size_t i = 0; i < Ngr; i++) {
      pixs.push_back(make_unique<IxNode>(cix));
    }
  }

  vector<R_xlen_t> beg(Ngr, 0), end(Ngr, 0);
  vector<unordered_map<uint_fast32_t, unique_ptr<RangeNode>>> out_nodess(Ngr);

  uint_fast32_t cix = child_ix(ix, spec.name);

  for (const SpecMatch& m: matches) {
    vector<NodeAccumulator> iaccs(Ngr);
    VarAccumulator ivacc(vacc.dedupe);

    for (size_t gi = 0; gi < Ngr; gi++) {
      const vector<Spec>& gspecs = get<1>(spec.groups[gi]);
      for (const Spec& s: gspecs) {
        add_node(*this, iaccs[gi], ivacc, spec, s, cix, m.obj);
      }
      end[gi] += iaccs[gi].nrows;
      // add index
      if (do_ix)
        pixs[gi]->push(beg[gi], end[gi], m.ix + 1, m.elem_name);
    }

    // move to out_nodes
    for (size_t gi = 0; gi < Ngr; gi++) {
      NodeAccumulator& iacc = iaccs[gi];
      auto& out_nodes = out_nodess[gi];
      while (!iacc.pnodes.empty()) {
        unique_ptr<Node>& ip = iacc.pnodes.front();
        auto oit = out_nodes.find(ip->ix);
        if (oit == out_nodes.end()) {
          unique_ptr<RangeNode> pr = make_unique<RangeNode>(ip->ix);
          pr->push(beg[gi], end[gi], move(ip));
          P("stacking new node:%s type:%s range:%ld-%ld Nnodes:%ld\n",
            full_name(pr->ix).c_str(), Rf_type2char(pr->type()),
            beg[gi], end[gi], pr->pnodes.size());
          out_nodes.emplace(pr->ix, move(pr));
        } else {
          P("stacking old node:%s type:%s range:%ld-%ld Nnodes:%ld\n",
            full_name(ip->ix).c_str(), Rf_type2char(ip->type()),
            beg[gi], end[gi], oit->second->pnodes.size()+1);
          oit->second->push(beg[gi], end[gi], move(ip));
        }
        iacc.pnodes.pop_front();
      }
      beg[gi] = end[gi];
    }

  }

  for (size_t ci = 0; ci < Ngr; ci++) {
    if (do_ix) {
      pixs[ci]->set_size(end[ci]);
      accs[ci].pnodes.push_front(move(pixs[ci]));
    }

    for (auto& onode: out_nodess[ci]) {
      onode.second->set_size(end[ci]);
      P("stacked node:%s type:%s, size:%ld\n",
        full_name(onode.second->ix).c_str(), Rf_type2char(onode.second->type()), onode.second->size());
      accs[ci].pnodes.push_front(move(onode.second));
    }

    if (stack_atomic || this->rep_to_max)
      accs[ci].nrows = max(accs[ci].nrows, end[ci]);
    else
      accs[ci].nrows = accs[ci].nrows * end[ci];
  }
  P("<<< gstack_nodes ---\n");

}

extern "C" SEXP C_unnest(SEXP x, SEXP lspec, SEXP dedupe,
                         SEXP stack_atomic, SEXP process_atomic,
                         SEXP process_unnamed_list,
                         SEXP cross_join) {
  if (TYPEOF(x) != VECSXP) {
	Rf_error("x must be a list vector");
  }

  Unnester unnester;
  unnester.dedupe = sexp2bool(dedupe);
  unnester.stack_atomic = sexp2bool(stack_atomic);
  unnester.stack_atomic_df = stack_atomic == R_NilValue;
  unnester.process_atomic = sexp2process(process_atomic);
  unnester.process_unnamed_list = sexp2unnamed(process_unnamed_list);
  unnester.rep_to_max = !sexp2bool(cross_join);

  return unnester.process(x, lspec);
}
