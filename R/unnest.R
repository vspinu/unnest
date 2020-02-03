
inset <- `[<-`

normalize_spec <- function(el) {
  if (!is.list(el))
    return(el)
  if (identical(class(el), "unnest.spec"))
    return(el)
  nms <- names(el)
  if (length(el) == 0)
    stop("Node spec is empty")
  if (is.null(nms)) {
    node <- el[[1]]
    children <- el[-1]
  } else {
    node <- el[["node"]]
    if (is.null(node)) {
      if (nms[[1]] == "") {
        node <- el[[1]]
        nms[[1]] <- "node"
      } else {
        stop(sprintf("Unspecified node name in spec node. Must be either unnamed first element or element with name 'node'.\n%s",
                     spec_el_to_char(inset(el, "children", NULL))),
             call. = FALSE)
      }
    }
    children <- c(el[!nzchar(nms)], el[["children"]])
    if (length(children) == 0)
      children <- NULL
  }
  if (!is.character(node))
    stop("Spec node name must be a character string")
  if (!all(is_lists <- sapply(children, is.list)))
    stop(sprintf("Children spec nodes must be lists. Not true for \n%s",
                 spec_el_to_char(el)))
  if (length(node) == 1)
    node <- strsplit(node, ":", fixed = TRUE)[[1]]
  named <- nzchar(nms)
  el <- structure(el[named], names = nms[named])
  el[["node"]] <- node[length(node)]
  el[["children"]] <- children
  if (length(node) > 1) {
    for (node in rev(node[-length(node)])) {
      if (grepl("^\\[[0-9]+\\]$", node))
        node <- as.integer(substr(node, 2, nchar(node) - 1))
      el <- unnest.spec(list(node = node,
                             children = list(unnest.spec(el))))
    }
  }
  unnest.spec(el)
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
s <- function(...) {
  normalize_spec(list(...))
}

#' @export
unnest <- function(list, spec = NULL) {
  .Call(C_unnest, list,  spec)
}
