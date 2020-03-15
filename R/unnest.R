
inset <- `[<-`

is.unnest.spec <- function(x) {
  identical(class(x), "unnest.spec")
}

unnest.spec <- function(x) {
  if (length(x) > 0)
    x <- x[!unlist(lapply(x, is.null), recursive = FALSE, use.names = FALSE)]
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
s <- function(node = NULL, ..., as = NULL,
              exclude = NULL, stack = FALSE,
              dedupe = NULL, sep = "/") {
  children <- list(...)
  children <- children[!sapply(children, is.null)]
  if (is.unnest.spec(node)) {
    children <- c(list(node), children)
    node <- NULL
  }
  if (!is.null(names(children)))
    stop(sprintf("spec children must be unnamed (not true for '%s')",
                 paste(names(children)[nzchar(names(children))], collapse = ",")))
  if (!all(isuspec <- sapply(children, is.unnest.spec)))
    stop("all spec children must be unnest.specs")
  if (!(is.null(node) || is.character(node) || is.numeric(node)))
    stop("Spec node must be NULL, numeric, or a character string")
  if (is.character(node) && length(node) == 1)
    node <- strsplit(paste0(node, sep), sep, fixed = TRUE)[[1]]
  el <- c(list(node = node),
          if (!is.null(as)) list(as = as),
          if (!is.null(exclude)) list(exclude = exclude),
          stack = stack,
          if (!is.null(dedupe)) list(dedupe = dedupe),
          if (length(children) > 0) list(children = children))
  if (length(node) > 1) {
    el_as <- el[["as"]]
    first <- TRUE
    for (node in rev(node)) {
      if (identical(node, ""))
        node <- NULL
      else if (grepl("^\\[[0-9]+\\]$", node))
        node <- as.integer(substr(node, 2, nchar(node) - 1))
      el1 <- unnest.spec(list(node = node,
                              ## special case: remove all nested names TOTHINK: better marker?
                              as = if (first) el_as
                                   else if (!is.null(el_as) && !is.null(node)) "",
                              stack = if (first && stack) stack,
                              exclude = if(first) exclude,
                              children = if(first) el[["children"]]
                                         else list(unnest.spec(el1))))
      first <- FALSE
    }
    el <- el1
  }
  unnest.spec(el)
}

#' @export
spec <- s

#' @export
unnest <- function(x, spec = NULL) {
  .Call(C_unnest, x,  spec)
}
