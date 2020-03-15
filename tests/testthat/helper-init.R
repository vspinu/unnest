
l <- base::list
unnestl <- function(x, s) as.list(unnest(x, s))
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
