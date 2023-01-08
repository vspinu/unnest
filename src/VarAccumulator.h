#ifndef UNNEST_VARACCUMULATOR_H
#define UNNEST_VARACCUMULATOR_H

#include "common.h"
#include "Spec.h"
#include <unordered_set>

struct VarAccumulator {
  bool dedupe = false;
  std::unordered_set<uint_fast32_t> acc;
  VarAccumulator(bool dedupe = false): dedupe(dedupe) {};

  bool has_var(uint_fast32_t ix) {
    if (dedupe) {
      const auto it = acc.find(ix);
      return (it != acc.end());
    } else {
      return false;
    }
  }

  void insert(uint_fast32_t ix) {
    if (dedupe) {
      acc.insert(ix);
    }
  }
};


#endif // UNNEST_VARACCUMULATOR_H
