
l <- base::list

unnestl <- function(x, ...) {
  out <- unnest(x, ...)
  if (is.data.frame(out)) as.list(out)
  else lapply(out, as.list)
}

x <- l(a = l(
         b = l(
           d = 2:1,
           e = l(
             f = 1:2,
             g = 4:6),
           c = l(
             l(a = 1, b = 1),
             l(a = 2, c = 2),
             l(a = 3, b = 3)))))

xx <- l(x, x)
xx[[1]][["a"]][["id"]] <- 1L
xx[[2]][["a"]][["id"]] <- 2L

y <- l(a = l(
         b = l(
           c = 10,
           d = 2:1,
           e = l(
             f = 1:2,
             g = 4:6))))
