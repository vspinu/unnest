context("BASIC")

library(unnest)
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

test_that("Spreading works", {

  x <- l(a = l(
           b = l(
             c = 10,
             d = 2:1,
             e = l(
               f = 1:2,
               g = 4:6))))

  expect_equal(unnest(x),
               structure(list(a.b.c = c(10, 10, 10, 10, 10, 10, 10, 10, 10,  10, 10, 10),
                              a.b.d = c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L),
                              a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
                              a.b.e.g = c(4L, 5L, 6L, 4L, 5L, 6L, 4L, 5L, 6L, 4L, 5L, 6L)),
                         class = "data.frame", row.names = c(NA, 12L)))

  expect_equal(unnest(x, s("a", s("b", s("e")))),
               structure(list(a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L),
                              a.b.e.g = c(4L, 5L, 6L, 4L, 5L, 6L)),
                         class = "data.frame", row.names = c(NA, 6L)))

  expect_equal(unnest(x, s("a", s("b", s("e", s("g"))))),
               structure(list(a.b.e.g = 4:6), class = "data.frame", row.names = c(NA, 3L)))

  expect_equal(nrow(unnest(x, s("a", s("X")))), 0)

})

test_that("Stacking of unnamed levels works", {

  xx <- l(x, x)
  xx[[1]][["a"]][["id"]] <- 1L
  xx[[2]][["a"]][["id"]] <- 2L

  expect_equal(unnest(x, s("a", s("b", s("c", s(stack = T))))),
               structure(list(a.b.c.a = c(1, 2, 3),
                              a.b.c.b = c(1, NA, 3),
                              a.b.c.c = c(NA, 2, NA)),
                         class = "data.frame", row.names = c(NA, 3L)))

  expect_equal(unnest(x, s("a", s("b", s("e", s("f")), s("c", s(stack = T))))),
               structure(list(a.b.c.a = c(1, 2, 3, 1, 2, 3),
                              a.b.c.b = c(1, NA, 3, 1, NA, 3),
                              a.b.c.c = c(NA, 2, NA, NA, 2, NA),
                              a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L)),
                         class = "data.frame", row.names = c(NA, 6L)))

  expect_equal(unnest(xx, s(stack = T, s("a", s("b", s("d")), s("id")))),
               structure(list(a.b.d = c(2L, 1L, 2L, 1L),
                              a.id = c(1L, 1L, 2L, 2L)),
                         class = "data.frame", row.names = c(NA, 4L)))

})

test_that("Stacking of named level works", {
  x <- l("1" = l(a = 1, b = l(c = 1:2)),
         "2" = l(a = 2, b = l(d = 2L)),
         "3" = l(a = 3, b = l(c = 3L)))
  expect_equal(unnestl(l(x = x), s("x", s(stack = T, s("a")))),
               list(x.a = c(1, 2, 3)))
  expect_equal(unnestl(x, s(stack = T, s("a"), s("b"))),
               list(a = c(1, 1, 2, 3),
                    b.c = c(1L, 2L, NA, 3L),
                    b.d = c(NA, NA, 2L, NA)))
})

test_that("Double stacking works", {

  xx <- l(x, x)
  xx[[1]][["a"]][["id"]] <- 1L
  xx[[2]][["a"]][["id"]] <- 2L

  spec <- s(stack = T, s("a", s("b", s("c", s(stack = T))), s("id")))
  expect_equal(unnest(xx[1], spec),
               structure(list(a.b.c.a = c(1, 2, 3),
                              a.b.c.b = c(1, NA, 3),
                              a.b.c.c = c(NA, 2, NA),
                              a.id    = c(1L, 1L, 1L)),
                         class = "data.frame", row.names = c(NA, 3L)))
  expect_equal(unnest(xx, spec),
               structure(list(a.b.c.a = c(1,  2, 3, 1, 2, 3),
                              a.b.c.b = c(1, NA, 3, 1, NA, 3),
                              a.b.c.c = c(NA,2, NA, NA, 2, NA),
                              a.id    = c(1L,1L, 1L, 2L, 2L, 2L)),
                         class = "data.frame", row.names = c(NA, 6L)))

  expect_equal(unnestl(xx[[1]], s("a", s("id"), s("b", s(stack = T)))),
               list(a.b = c(2L, 1L, NA, NA, NA, NA, NA, NA, NA),
                    a.b.1.a = c(NA, NA, NA, NA, NA, NA, NA, NA, 1),
                    a.b.1.b = c(NA, NA, NA, NA, NA, NA, NA, NA, 1),
                    a.b.2.a = c(NA, NA, NA, NA, NA, NA, NA, NA, 2 ),
                    a.b.2.c = c(NA, NA, NA, NA, NA, NA, NA, NA, 2),
                    a.b.3.a = c(NA, NA, NA, NA, NA, NA, NA, NA, 3),
                    a.b.3.b = c(NA, NA, NA, NA, NA, NA, NA, NA, 3),
                    a.b.f   = c(NA, NA, 1L, 2L, 1L, 2L, 1L, 2L, NA),
                    a.b.g   = c(NA, NA, 4L, 5L, 6L, 4L, 5L, 6L, NA),
                    a.id    = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)))

  expect_equal(rbind(unnest(xx[[1]], s("a", s("id"), s("b", s(stack = T)))),
                     unnest(xx[[2]], s("a", s("id"), s("b", s(stack = T))))),
               unnest(xx, s(stack = T, s("a", s("id"), s("b", s(stack = T))))))

})


test_that("Renaming works", {
  expect_equal(names(unnest(x, s("a", name = "A", s("b", s("e", name = "E", s("g")))))),
               "A.b.E.g")
  out <- unnest(x, s("a",
                     s("b",
                       s("e", name = "E", s("f", name = "F")),
                       s("c", name = "C", s(stack = T)))))
  expect_equal(names(out),  c("a.b.C.a", "a.b.C.b", "a.b.C.c", "a.b.E.F"))
  expect_equal(unnest(x, s("a", s("b", name = "", s("e", name = "E", s("g"))))),
               structure(list(a.E.g = 4:6), class = "data.frame", row.names = c(NA, 3L)))
})

test_that("Short form node spec works", {
  expect_equal(unnestl(x, s("a:b:d", name = "aaa")),
               list(aaa = 2:1))
  expect_equal(unnestl(x, s("a::d")),
               list(a.b.d = 2:1))
  expect_equal(unnestl(x, s("a::c:[3]")),
               list(a.b.c.3.a = 3, a.b.c.3.b = 3))
  expect_equal(unnestl(x, s("a::[3]:[3]")),
               list(a.b.c.3.a = 3, a.b.c.3.b = 3))
  expect_equal(unnestl(x, s("[1]:[1]:[3]:[3]")),
               list(a.b.c.3.a = 3, a.b.c.3.b = 3))
})

test_that("Exclusion works", {
  expect_equal(unnest(x, s("a", s("b", s(exclude = c("d", "c"))))),
               structure(list(a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L),
                              a.b.e.g = c(4L, 5L, 6L, 4L, 5L, 6L)),
                         class = "data.frame", row.names = c(NA, 6L)))
  expect_equal(nrow(unnest(x, s("a", s("b", s(exclude = c("e", "d", "c")))))), 0)
})

test_that("Superfluous spec nesting is honored", {
  expect_equal(nrow(unnest(x, s("a:b:d:e"))), 0)
})

test_that("Integer nodes work", {
  expect_equal(unnestl(x, s("a:b:c", s(2))),
               list(a.b.c.2.a = 2, a.b.c.2.c = 2))
  expect_equal(unnestl(x, s("a:b:e", s(2))),
               list(a.b.e.g = 4:6))
  expect_equal(unnestl(x, s("a:b:e", s(1), s(2))),
               list(a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L),
                    a.b.e.g = c(4L, 5L, 6L, 4L, 5L, 6L)))
  expect_equal(unnestl(x, s("a:b",
                            s("e", s(2)),
                            s(3, s(3)))),
               list(a.b.c.3.a = c(3, 3, 3),
                    a.b.c.3.b = c(3, 3, 3),
                    a.b.e.g = 4:6))
})
