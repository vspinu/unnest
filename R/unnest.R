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

#' @title Unnest nested lists
#'
#' @description Unnest spec is a nested list with the same structure as the
#'   nested json. It specifies concisely how the deeply nested components ought
#'   to be unnested. `s()` is a shorthand for `spec()` and returns the canonical
#'   spec (a spec which can be consumed directly by the C++ routines).
#'
#' @rdname unnest
#' @param selector A shorthand syntax for an `include` selector. When a list,
#'   each element of the list is expanded into the `include` element at the
#'   respective level. When a string, it is expanded into a list according to
#'   the following rules:
#'
#'   \enumerate{ \item When selector is of length 1 and contains "/" characters
#'      it is split with the "/" separator. For instance `s(c("a", "b"), ...)`,
#'      `s("a/b", ...)` and `s("a", s("b", ...))` are all converted to the
#'      canonical `s(include = "a", s(include = "b", ...))`. Components
#'      consisting entirely of digits are converted to integers. For example
#'      `s("a/2/b" ...)` is equivalent to `s("a", s(2, s("b", ...)))` \item Each
#'      element of the resulting from the previous step vector is split with
#'      `,`. Thus `s("a/b,c/d")` is equivalent to `s("a", s(include = c("b",
#'      "c"), s("d", ...)))` }
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
#'   processing (the default), "asis" to return the entire element "as is" in a
#'   list column, "paste" to paste elements together into a character column.
#' @return `spec()`: a canonical spec - a list consumed by C++ unnesting
#'   routines.
#' @examples
#'
#' ## `s()` returns a canonical spec list
#' s("a")
#' s("a//c2")
#' s("a/2/c2,cid")
#'
#' @export
s <- function(selector = NULL, ..., as = NULL,
              children = NULL, groups = NULL,
              include = NULL, exclude = NULL,
              stack = NULL, process = NULL) {
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
          if (!is.null(stack))  list(stack = stack),
          if (!is.null(process))  list(process = process),
          if (length(children) > 0) list(children = children),
          if (!is.null(groups)) list(groups = groups))

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
        include = include,
        exclude = exclude,
        children = if(first) el[["children"]] else list(tel),
        groups = groups))
    include <- exclude <- stack <- process <- groups <- NULL
    first <- FALSE
  }
  el <- tel
  unnest.spec(el)
}

#' @rdname unnest
#' @export
spec <- s

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


#' @param x a nested list to unnest
#' @param spec spec to use for unnesting. See [`spec()`].
#' @param dedupe whether to dedupe repeated elements. If TRUE, if a node is
#'   visited for a second time and is not explicitly declared in the `spec` the
#'   node is skipped. This is particularly useful with `group`ed specs.
#' @param stack_atomic Whether atomic leaf vectors should be stacked or not.
#' @param process_atomic Process spec for atomic leaf vectors. (Unstable: Might
#'   be removed in the future as the use case is not clear.)
#' @param cross_join Specifies how the results from sibling nodes are joined
#'   (`cbind`) together. The shorter data.frames (in terms o number of rows) can
#'   be either recycled to the max number of rows across all components as with
#'   standard R's recycling (`cross_join = FALSE`). Or, with `cross_join =
#'   TRUE`, the results are cross joined (aka form all combinations of rows
#'   across joined components). `cross_join = TRUE` is the default because of no
#'   data loss and it is more conducive for earlier error detection with
#'   incorrect specs.
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
#' ## processing asis
#' str(unnest(xxx, s(stack = "id",
#'                   s("a/b/y", process = "asis"),
#'                   s("a/c", process = "asis"))))
#' str(unnest(xxx, s(stack = "id", s("a/b/", process = "asis"))))
#' str(unnest(xxx, s(stack = "id", s("a/b", process = "asis"))))
#'
#' ## processing paste
#' str(unnest(x, s("a/b/y", process = "paste")))
#' str(unnest(xxx, s(stack = TRUE, s("a/b/", process = "paste"))))
#' str(unnest(xxx, s(stack = TRUE, s("a/b", process = "paste"))))
#'
#' @export
unnest <- function(x, spec = NULL, dedupe = FALSE,
                   stack_atomic = FALSE,
                   process_atomic = NULL,
                   cross_join = TRUE) {
  if (!is.null(spec) && !inherits(spec, "unnest.spec")) {
    stop("`spec` argument must be of class `unnest.spec`", call. = FALSE)
  }
  out <- .Call(C_unnest, x, spec, dedupe,
               stack_atomic, process_atomic,
               cross_join)
  switch(getOption("unnest.return.type", "data.frame"),
         data.frame = out,
         tibble = convert_to_tible(out),
         data.table = convert_to_dt(out),
         stop("Invalid `unnest.return.type` option (%s). Valid types are `data.frame`, `data.table` and `tibble`"))
}
