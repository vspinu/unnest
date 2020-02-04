
inset <- `[<-`

is.unnest.spec <- function(x) {
  identical(class(x), "unnest.spec")
}

unnest.spec <- function(x) {
  structure(x, class = "unnest.spec")
}

#' @export
str.unnest.spec <- function(object, nest.lev = 0, no.list = FALSE, ...) {
  cat("<unnest.spec>\n")
  utils:::str.default(object, nest.lev = nest.lev, no.list = T, ...)
}

#' @export
print.unnest.spec <- function(x, ...) {
  str(x, give.head = FALSE, no.list = TRUE, give.attr = FALSE)
}

#' @export
s <- function(node = NULL, ..., name = NULL, stack = FALSE) {
  children <- list(...)
  if (is.unnest.spec(node)) {
    children <- c(list(node), children)
    node <- NULL
  }
  if (!is.null(names(children)))
    stop(sprintf("spec children must be unnamed (not true for '%s')",
                 paste(names(children)[nzchar(names(children))], collapse = ",")))
  if (!all(isuspec <- sapply(children, is.unnest.spec)))
    stop("all spec children must be unnest.specs")
  if (!is.null(node) && !is.character(node))
    stop("Spec node must be NULL or a character string")
  if (length(node) == 1)
    node <- strsplit(node, ":", fixed = TRUE)[[1]]
  el <- list(node = node,
             name = name,
             stack = stack,
             children = children)
  if (length(node) > 1) {
    el[["node"]] <- node[[length(node)]]
    new_name <- el[["name"]]
    for (node in rev(node[-length(node)])) {
      if (identical(node, ""))
        node <- NULL
      else if (grepl("^\\[[0-9]+\\]$", node))
        node <- as.integer(substr(node, 2, nchar(node) - 1))
      el <- unnest.spec(list(node = node,
                             ## special case: remove all nested names TOTHINK: better marker?
                             name = if (!is.null(new_name)) "",
                             children = list(unnest.spec(el))))
      if (is.null(el[["name"]]))
        el[["name"]] <- NULL
    }
  }
  if (is.null(el[["name"]]))
    el[["name"]] <- NULL
  unnest.spec(el)
}

#' @export
spec <- s

#' @export
unnest <- function(x, spec = NULL) {
  .Call(C_unnest, x,  spec)
}
