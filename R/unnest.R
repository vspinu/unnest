
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
  utils:::str.default(object, nest.lev = nest.lev, no.list = T, ...)
}

#' @export
print.unnest.spec <- function(x, ...) {
  str(x, give.head = FALSE, no.list = TRUE, give.attr = FALSE)
}

#' @export
s <- function(selector = NULL, ..., as = NULL,
              children = NULL, groups = NULL,
              include = NULL, exclude = NULL,
              dedupe = NULL, stack = NULL) {
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
          if (!is.null(dedupe)) list(dedupe = dedupe),
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
        ## special case: remove all nested names TOTHINK: better marker?
        as = if (first) el[["as"]]
             else if (!is.null(el[["as"]])) "",
        stack = stack,
        include = include,
        exclude = exclude,
        dedupe = dedupe,
        children = if(first) el[["children"]] else list(tel),
        groups = groups))
    dedupe <- include <- exclude <- stack <- groups <- NULL
    first <- FALSE
  }
  el <- tel
  unnest.spec(el)
}

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

#' @export
unnest <- function(x, spec = NULL) {
  if (!is.null(spec) && !inherits(spec, "unnest.spec")) {
    stop("`spec` argument must be either `unnest.spec` or `unnest.pspec`", call. = FALSE)
  }
  out <- .Call(C_unnest, x,  spec)
  switch(getOption("unnest.return.type", "data.frame"),
         data.frame = out,
         tibble = convert_to_tible(out),
         data.table = convert_to_dt(out),
         stop("Invalid `unnest.return.type` option (%s). Valid types are `data.frame`, `data.table` and `tibble`"))
}
