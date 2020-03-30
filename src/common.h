
#ifndef UNNEST_COMMON_H
#define UNNEST_COMMON_H

#include <algorithm>
#include <cstring>
#include <memory>
#include <numeric>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
#include <tuple>
#include "Rinternals.h"

using namespace std;

/* #define DEBUG */

#ifdef DEBUG
#define P(...) printf(__VA_ARGS__)
#else
#define P(...)
#endif

SEXP rep_vector(SEXP x, R_xlen_t N);
SEXP make_na_vector(SEXPTYPE type, R_xlen_t len);
void fill_vector(SEXP source, SEXP target, R_xlen_t from, R_xlen_t to);

inline bool is_char_in_strvec(SEXP ch, SEXP str) {
  if (str == R_NilValue)
    return false;
  bool has = false;
  R_len_t N = XLENGTH(str);
  for (R_len_t i = 0; i < N; i++) {
    if (ch == STRING_ELT(str, i)) {
      has = true;
      break;
    }
  }
  return has;
}

template <typename T>
vector<size_t> orderix(const vector<T>& v) {
  vector<size_t> idx(v.size());
  iota(idx.begin(), idx.end(), 0);
  stable_sort(idx.begin(), idx.end(),
              [&v](size_t i1, size_t i2) {return v[i1] < v[i2];});
  return idx;
}

/* make_unique for c++11 from https://stackoverflow.com/a/17902439/453735 */

#if __cplusplus == 201103L

#include <cstddef>
#include <type_traits>

namespace std {
template<class T> struct _Unique_if {
  typedef unique_ptr<T> _Single_object;
};

template<class T> struct _Unique_if<T[]> {
  typedef unique_ptr<T[]> _Unknown_bound;
};

template<class T, size_t N> struct _Unique_if<T[N]> {
  typedef void _Known_bound;
};

template<class T, class... Args>
typename _Unique_if<T>::_Single_object
make_unique(Args&&... args) {
  return unique_ptr<T>(new T(std::forward<Args>(args)...));
}

  template<class T>
  typename _Unique_if<T>::_Unknown_bound
  make_unique(size_t n) {
    typedef typename remove_extent<T>::type U;
    return unique_ptr<T>(new U[n]());
  }

    template<class T, class... Args>
    typename _Unique_if<T>::_Known_bound
    make_unique(Args&&...) = delete;
}

#endif

#endif // UNNEST_COMMON_H
