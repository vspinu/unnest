
rmap <- function(.x, .fpre = NULL, .fpost = NULL, ...) {
  .fpost <- if (is.null(.fpost)) identity
             else purrr::as_mapper(.fpost, ...)
  .fpre <- if (is.null(.fpre)) identity
             else purrr::as_mapper(.fpre, ...)
  worker <- function(x) {
    x <- .fpre(x)
    if (is.recursive(x)) {
      .fpost(lapply(x, worker))
    } else {
      .fpost(x)
    }
  }
  .fpost(lapply(.fpre(.x), worker))
}

paste_comma <- function(...) {
  paste(..., sep = ", ", collapse = ", ")
}

spec_el_to_char <- function(el) {
  paste(capture.output(str(el, give.head = FALSE, no.list = TRUE)), collapse = "\n")
}
