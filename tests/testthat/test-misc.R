context("Misc")

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

test_that("Exclusion works", {
  expect_equal(unnest(x, s("a", s("b", s(exclude = c("d", "c"))))),
               structure(list(a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L),
                              a.b.e.g = c(4L, 5L, 6L, 4L, 5L, 6L)),
                         class = "data.frame", row.names = c(NA, 6L)))
  expect_equal(nrow(unnest(x, s("a", s("b", s(exclude = c("e", "d", "c")))))), 0)
})

test_that("Superfluous spec nesting is honored", {
  expect_equal(nrow(unnest(x, s("a/b/d/e"))), 0)
})

test_that("Integer nodes work", {
  expect_equal(unnestl(x, s("a/b/c", s(2))),
               list(a.b.c.2.a = 2, a.b.c.2.c = 2))
  expect_equal(unnestl(x, s("a/b/e", s(2))),
               list(a.b.e.g = 4:6))
  expect_equal(unnestl(x, s("a/b/e", s(1), s(2))),
               list(a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L),
                    a.b.e.g = c(4L, 5L, 6L, 4L, 5L, 6L)))
  expect_equal(unnestl(x, s("a/b",
                            s("e", s(2)),
                            s(3, s(3)))),
               list(a.b.c.3.a = c(3, 3, 3),
                    a.b.c.3.b = c(3, 3, 3),
                    a.b.e.g = 4:6))
})

test_that("Multi-chain works", {
  u <- unnestl(x, s("a", s("b/d"), s("b/c/[3]/a", as = "x")))
  expect_equal(u, l(a.b.d = 2:1, a.x = c(3, 3)))
  expect_equal(u, unnestl(x, s(s("b/d"), s("b/c/[3]/a", as = "x"))))
  expect_equal(unnestl(x, s("a/b", s("d"), s("c/[3]/a", as = "x"))),
               l(a.b.d = 2:1, a.b.x = c(3, 3)))
})
