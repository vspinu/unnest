#ifndef UNNEST_VARACCUMULATOR_H
#define UNNEST_VARACCUMULATOR_H

#include "common.h"
#include "Spec.h"
#include <unordered_set>

struct VarAccumulator {
  bool accumulate = false;
  unordered_set<uint_fast32_t> acc;
  VarAccumulator(bool accumulate = false): accumulate(accumulate) {};

  bool has_var(uint_fast32_t ix) {
    if (accumulate) {
      const auto it = acc.find(ix);
      return (it != acc.end());
    } else {
      return false;
    }
  }

  bool add_var(uint_fast32_t ix) {
    P("ACCVAR: %lu [acc: %d]\n", ix, accumulate);
    if (accumulate) {
      acc.insert(ix);
    }
  }
};


#endif // UNNEST_VARACCUMULATOR_H
