
#ifndef UNNEST_H
#define UNNEST_H

#include "Rinternals.h"

SEXP rep_vector(SEXP x, R_xlen_t N);

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


#endif // UNNEST_H
