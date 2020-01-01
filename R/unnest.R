
## e <- list

## spec <-
##     list(incomes = e("serviceRequest", 
##                      name = NULL, 
##                      e("data",
##                        name = NULL, 
##                        e("loanApplication", 
##                          name = "la",
##                          e(node = "id",
##                            name = "la.id"), 
##                          e(node = "households:applicants:[1]:incomes",
##                            matches = ".*",
##                            arange = "vertical")))))

## normalize_spec(spec$incomes)


## unnest1 <- function(list, spec) {
##     out <- list()
##     names <- 
##     for ()
## } 


#' @export
unnest <- function(list) {
  .Call(C_unnest, list)
}
