#include "unnest.h"

void Unnester::stack_nodes(NodeAccumulator& acc, VarAccumulator& vacc,
                           const Spec& spec, uint_fast32_t ix,
                           const vector<SpecMatch>& matches) {
  P(">>> stack_nodes ---\n");
  size_t N = matches.size();

  R_xlen_t beg = 0, end=0;
  unordered_map<uint_fast32_t, unique_ptr<RangeNode>> out_nodes;

  bool do_ix = (spec.name != R_NilValue && spec.name != R_BlankString);
  unique_ptr<IxNode> pix;
  if (do_ix)
    pix = make_unique<IxNode>(child_ix(ix, CHAR(spec.name)));

  int i = 1;
  for (const SpecMatch& m: matches) {
    NodeAccumulator iacc;
    VarAccumulator ivacc(vacc.accumulate);


    dispatch_match_to_child(iacc, ivacc, spec, ix, m);

    end += iacc.nrows;

    // add index
    if (do_ix)
      pix->push(beg, end, i++);

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

  for (auto& on: out_nodes) {
    on.second->set_size(end);
    P("stacked node:%s type:%s, size:%ld\n",
      full_name(on.second->ix).c_str(), Rf_type2char(on.second->type()), on.second->size());
    acc.pnodes.push_front(move(on.second));
  }

  acc.nrows *= end;
  P("<<< stack_nodes ---\n");

}

void Unnester::stack_nodes(vector<NodeAccumulator>& accs, VarAccumulator& vacc,
                           const Spec& spec, uint_fast32_t ix,
                           const vector<SpecMatch>& matches) {

  if (accs.size() == 0) return;

  P(">>> gstack_nodes ---\n");

  size_t Ngr = spec.groups.size(), N = matches.size();;
  if (accs.size() != Ngr)
    Rf_error("Internal: Invalid grouped stack. Accumulator size (%ld) and spec size (%l) mismatch.",
             accs.size(), Ngr);
  bool do_ix = (spec.name != R_NilValue && spec.name != R_BlankString);

  if (spec.children.size() > 0)
    Rf_error("Supplying both children and groups is not yet supported");

  vector<unique_ptr<IxNode>> pixs;
  if (do_ix) {
    uint_fast32_t cix = child_ix(ix, CHAR(spec.name));
    for (size_t i = 0; i < Ngr; i++) {
      pixs.push_back(make_unique<IxNode>(cix));
    }
  }

  vector<R_xlen_t> beg(Ngr, 0), end(Ngr, 0);
  vector<unordered_map<uint_fast32_t, unique_ptr<RangeNode>>> out_nodess(Ngr);

  int i = 1;
  for (const SpecMatch& m: matches) {
    vector<NodeAccumulator> iaccs(Ngr);
    VarAccumulator ivacc(vacc.accumulate);

    for (size_t gi = 0; gi < Ngr; gi++) {
      const vector<Spec>& gspecs = get<1>(spec.groups[gi]);
      for (const Spec& s: gspecs) {
        add_node(*this, iaccs[gi], ivacc, s, ix, m.obj);
      }
      end[gi] += iaccs[gi].nrows;
      // add index
      if (do_ix)
        pixs[gi]->push(beg[gi], end[gi], i);
    }

    i++;

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

    for (auto& on: out_nodess[ci]) {
      on.second->set_size(end[ci]);
      P("stacked node:%s type:%s, size:%ld\n",
        full_name(on.second->ix).c_str(), Rf_type2char(on.second->type()), on.second->size());
      accs[ci].pnodes.push_front(move(on.second));
    }

    accs[ci].nrows *= end[ci];
  }
  P("<<< gstack_nodes ---\n");

}

extern "C" SEXP C_unnest(SEXP x, SEXP lspec) {
  SEXPTYPE type = TYPEOF(x);
  if (TYPEOF(x) != VECSXP) {
	Rf_error("x must be a list vector");
  }

  Unnester unnester;

  return unnester.process(x, lspec);
}
