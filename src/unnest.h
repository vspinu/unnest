
#ifndef UNNEST_H
#define UNNEST_H

#include "Rinternals.h"

#define DEBUG

#ifdef DEBUG
#define P(...) printf(__VA_ARGS__)
#else
#define P(...)
#endif


SEXP rep_vector(SEXP x, R_xlen_t N);

SEXP make_na_vector(SEXPTYPE type, R_xlen_t len);

void fill_vector(SEXP source, SEXP target, R_xlen_t from, R_xlen_t to);

/* #define USE_SPP */

/* #ifdef USE_SPP */
/* #include <sparsepp/spp.h> */
/* /// comment from text2vec: */
/* // spp has calls to 'exit' on failure, which upsets R CMD check. */
/* // We won't bump into them during normal test execution so just override */
/* // it in the spp namespace before we include 'sparsepp'. */
/* // https://github.com/hadley/testthat/blob/c7e8330867645c174f9a286d00eb0036cea78b0c/inst/include/testthat/testthat.h#L44-L50 */
/* // https://stackoverflow.com/questions/43263880/no-ambiguous-reference-error-even-after-using-namespace-directive/43294812 */
/* namespace spp { */
/* inline void exit(int status) throw() {} */
/* } */
/* /\* typedef spp::sparse_hash_map<const char*, uint32_t>::iterator hashmap_char_iter; *\/ */
/* template<typename T1, typename T2> */
/* using hashmap = spp::sparse_hash_map<T1, T2>; */
/* #else */
/* #include <unordered_map> */
/* template<typename T1, typename T2> */
/* using hashmap = std::unordered_map<T1, T2>; */
/* #endif */

/* make_unique for c++11 from https://stackoverflow.com/a/17902439/453735 */

#if __cplusplus == 201103L

#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>

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

#endif // UNNEST_H
