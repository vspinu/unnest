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
  cat(sprintf("<%s>\n", paste(class(object), collapse = ",")))
  utils::str(unclass(object), nest.lev = nest.lev, no.list = T, ...)
}

#' @export
print.unnest.spec <- function(x, ...) {
  str(x, give.head = FALSE, no.list = TRUE, give.attr = FALSE)
}

#' @title Unnest spec
#'
#' @description Unnest spec is a nested list with the same structure as the
#'   nested json. It specifies how the deeply nested lists ought to be
#'   unnested. `spec()` is a handy constructor for spec lists. `s()` is a
#'   shorthand alias for `spec()`.
#'
#' @rdname spec
#' @param selector A shorthand syntax for an `include` parameter. Can be a list
#'   or a character vector.
#'
#' \enumerate{
#'
#'    \item When `selector` is a list or a character vector with length greater
#'    than 1, each element is an include parameter at the corresponding
#'    level. For example `s(c("a", "b"), ...)` is equivalent to `s(include =
#'    "a", s(include = "b", ...))`
#'
#'    \item When `selector` is a character of length 1 and contains "/"
#'    characters it is split with "/" first. For instance `s(c("a", "b"), ...)`,
#'    `s("a/b", ...)` and `s("a", s("b", ...))` are all equivalent to the
#'    canonical `s(include = "a", s(include = "b", ...))`. Components consisting
#'    entirely of digits are converted to integers. For example `s("a/2/b" ...)`
#'    is equivalent to `s("a", s(2, s("b", ...)))`
#'
#'    \item Multiple `include` fields can be separated with `,`. For example
#'      `s("a/b,c/d")` is equivalent to `s("a", s(include = c("b", "c"), s("d",
#'      ...)))`
#'
#' }
#' @param as name for this field in the extracted data.frame
#' @param children,... Unnamed list of children spec. `...` is merged into
#'   `children`. `children` is part of the canonical spec.
#' @param groups Named list of specs to be processed in parallel. The return
#'   value is a named list of unnested data.frames. The results is the same as
#'   when each spec is `unnest`ed separately except that `dedupe` parameter of
#'   `unnest()` will work across groups and execution is faster because the
#'   nested list is traversed once regardless of the number of groups.
#' @param include,exclude A list, a numeric vector or a character vector
#'   specifying components to include or exclude. A list can combine numeric
#'   indexes and character elements to extract.
#' @param stack Whether to stack this node (TRUE) or to spread it (FALSE). When
#'   `stack` is a string an index column is created with that name.
#' @param process Extra processing step for this element. Either NULL for no
#'   processing (the default), "as_is" to return the entire element in a list
#'   column, "paste" to paste elements together into a character column.
#' @param default Default value to insert if the `include` specification hasn't
#'   matched.
#' @return `s()`: a canonical spec - a list consumed by C++ unnesting routines.
#' @examples
#'
#' s("a")
#' s("a//c2")
#' s("a/2/c2,cid")
#'
#' @export
spec <- function(selector = NULL, ..., as = NULL,
              children = NULL, groups = NULL,
              include = NULL, exclude = NULL,
              stack = NULL, process = NULL,
              default = NULL) {
  children <- c(children, list(...))
  children <- children[!sapply(children, is.null)]
  if (is.unnest.spec(selector)) {
    children <- c(list(selector), children)
    selector <- NULL
  }
  if (!is.null(names(children)))
    stop(sprintf("spec children must be unnamed (not true for '%s')",
                 paste(names(children)[nzchar(names(children))], collapse = ",")))
  if (!all(isuspec <- sapply(children, is.unnest.spec)))
    stop("all spec children must be unnest.specs")
  if (!(is.null(selector) || is.character(selector) || is.numeric(selector)))
    stop("Spec selector must be NULL, integer, or a character string")
  if (!is.null(groups))
    if (!is.list(groups) || is.null(names(groups)))
      stop("Groups argument must be a list of named specs")

  if (is.character(selector)) {
    if (length(selector) == 1) {
      sel <- strsplit(paste0(selector, "/"), "/", fixed = TRUE)[[1]]
      selector <- as.list(sel)
      is_int <- grepl("^[0-9]+$", sel)
      selector[is_int] <- as.integer(sel[is_int])
      selector[!is_int] <- strsplit(paste0(sel[!is_int], ","), ", *")
    } else {
      selector <- strsplit(paste0(selector, ","), ", *")
    }
  }

  el <- c(list(),
          if (!is.null(as)) list(as = as),
          if (!is.null(include)) list(include = include),
          if (!is.null(exclude)) list(exclude = exclude),
          if (!is.null(stack)) list(stack = stack),
          if (!is.null(process)) list(process = process),
          if (!is.null(default)) list(default = default),
          if (length(children) > 0) list(children = children),
          if (length(groups) > 0) list(groups = groups))

  first <- TRUE
  tel <- el
  for (sel in rev(selector)) {
    if (identical(sel, ""))
      sel <- NULL
    include <-
      if (first)
        if (is.null(include)) sel
        else if (is.null(sel)) include
        else if (identical(class(sel), class(include))) c(sel, include)
        else c(as.list(sel), as.list(include))
      else sel
    tel <-
      unnest.spec(list(
        ## special case: remove all nested names
        ## TOTHINK: better marker?
        as = if (first) el[["as"]]
             else if (!is.null(el[["as"]])) "",
        stack = stack,
        process = process,
        default = default,
        include = include,
        exclude = exclude,
        children = if(first) el[["children"]] else list(tel),
        groups = groups))
    include <- exclude <- stack <- process <- default <- groups <- NULL
    first <- FALSE
  }
  el <- tel
  unnest.spec(el)
}

#' @rdname spec
#' @export
s <- spec

convert_to_tible <- function(x) {
  if (!is.data.frame(x))
    lapply(x, convert_to_tible)
  else {
    names(x) <- make.names(names(x))
    tibble::as_tibble(x)
  }
}

convert_to_dt <- function(x) {
  if (!is.data.frame(x))
    lapply(x, convert_to_dt(x))
  else {
    data.table::as.data.table(x)
  }
}


#' Unnest lists
#'
#' Unnest nested lists into a flat data.frames.
#'
#' @param x a nested list to unnest
#' @param spec spec to use for unnesting. See [`spec()`].
#' @param dedupe whether to dedupe repeated elements. If TRUE, if a node is
#'   visited for a second time and is not explicitly declared in the `spec` the
#'   node is skipped. This is particularly useful with `group`ed specs.
#' @param stack_atomic Whether atomic leaf vectors should be stacked or not. If
#'   NULL, the default, data.frame vectors are stacked, all others are spread.
#' @param process_atomic Process spec for atomic leaf vectors. Either NULL for
#'   no processing (the default), "as_is" to return the entire element in a list
#'   column, "paste" to paste elements together into a character column.
#' @param process_unnamed_lists How to process unnamed lists. Can be one of
#'   "as_is" - return a list column, "exclude" - drop these elements unless they
#'   are explicitly included in the spec, "paste" - return a character column,
#'   "stack" - automatically stack. If NULL (the default), do nothing - process
#'   them normally according to the specs.
#' @param cross_join Specifies how the results from sibling nodes are joined
#'   (`cbind`ed) together. The shorter data.frames (fewer rows) can be either
#'   recycled to the max number of rows across all joined components with
#'   `cross_join = FALSE`. Or, the results are cross joined (produce all
#'   combinations of rows across all components) with `cross_join =
#'   TRUE`. `cross_join = TRUE` is the default because of no data loss and it is
#'   more conducive for earlier error detection with incorrect specs
#'
#' @return A `data.frame`, `data.table` or a `tibble` as specified by the option
#'   `unnest.return.type`. Defaults to `data.frame`.
#' @examples
#'
#' x <- list(a = list(b = list(x = 1, y = 1:2, z = 10),
#'                    c = list(x = 2, y = 100:102)))
#' xxx <- list(x, x, x)
#'
#' ## spreading
#' unnest(x, s("a"))
#' unnest(x, s("a"), stack_atomic = TRUE)
#' unnest(x, s("a/b"), stack_atomic = TRUE)
#' unnest(x, s("a/c"), stack_atomic = TRUE)
#' unnest(x, s("a"), stack_atomic = TRUE, cross_join = TRUE)
#' unnest(x, s("a//x"))
#' unnest(x, s("a//x,z"))
#' unnest(x, s("a/2/x,y"))
#'
#' ## stacking
#' unnest(x, s("a/", stack = TRUE))
#' unnest(x, s("a/", stack = TRUE, as = "A"))
#' unnest(x, s("a/", stack = TRUE, as = "A"), stack_atomic = TRUE)
#' unnest(x, s("a/", stack = "id"), stack_atomic = TRUE)
#' unnest(x, s("a/", stack = "id", as = ""), stack_atomic = TRUE)
#'
#' unnest(xxx, s(stack = "id"))
#' unnest(xxx, s(stack = "id"), stack_atomic = TRUE)
#' unnest(xxx, s(stack = "id", s("a/b/y/", stack = TRUE)))
#'
#' ## exclusion
#' unnest(x, s("a/b/", exclude = "x"))
#'
#' ## dedupe
#' unnest(x, s("a", s("b/y"), s("b")), stack_atomic = TRUE)
#' unnest(x, s("a", s("b/y"), s("b")), dedupe = TRUE, stack_atomic = TRUE)
#'
#' ## grouping
#' unnest(xxx, stack_atomic = TRUE,
#'        s(stack = TRUE,
#'          groups = list(first = s("a/b/x,y"),
#'                        second = s("a/b"))))
#'
#' unnest(xxx, stack_atomic = TRUE, dedupe = TRUE,
#'        s(stack = TRUE,
#'          groups = list(first = s("a/b/x,y"),
#'                        second = s("a/b"))))
#'
#' ## processing as_is
#' str(unnest(xxx, s(stack = "id",
#'                   s("a/b/y", process = "as_is"),
#'                   s("a/c", process = "as_is"))))
#' str(unnest(xxx, s(stack = "id", s("a/b/", process = "as_is"))))
#' str(unnest(xxx, s(stack = "id", s("a/b", process = "as_is"))))
#'
#' ## processing paste
#' str(unnest(x, s("a/b/y", process = "paste")))
#' str(unnest(xxx, s(stack = TRUE, s("a/b/", process = "paste"))))
#' str(unnest(xxx, s(stack = TRUE, s("a/b", process = "paste"))))
#'
#' ## default
#' unnest(x, s("a/b/c/", s("b", default = 100)))
#' unnest(x, s("a/b/c/", stack = "ix", s("b", default = 100)))
#'
#' @export
unnest <- function(x, spec = NULL, dedupe = FALSE,
                   stack_atomic = NULL,
                   process_atomic = NULL,
                   process_unnamed_lists = NULL,
                   cross_join = TRUE) {
  if (!is.null(spec) && !inherits(spec, "unnest.spec")) {
    stop("`spec` argument must be of class `unnest.spec`", call. = FALSE)
  }
  out <- .Call(C_unnest, x, spec, dedupe, stack_atomic,
               process_atomic, process_unnamed_lists,
               cross_join)
  switch(getOption("unnest.return.type", "data.frame"),
         data.frame = out,
         tibble = convert_to_tible(out),
         data.table = convert_to_dt(out),
         stop("Invalid `unnest.return.type` option (%s). Valid types are `data.frame`, `data.table` and `tibble`"))
}
