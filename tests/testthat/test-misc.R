context("Misc")

test_that("Automatic treatment of data.frames works",  {
  dd <- list(a = data.frame(b = 1:2, c = 11:12, d = c("21", "22")))
  expect_equal(unnestl(dd, s("a")),
               list(a.b = 1:2, a.c = 11:12, a.d = c("21", "22")))
  expect_equal(unnestl(dd, s("a"), stack_atomic = F),
               list(a.b = 1L, a.b.2 = 2L, a.c = 11L, a.c.2 = 12L, a.d = "21", a.d.2 = "22"))
  expect_equal(unnestl(dd, s("a/b,d")),
               list(a.b = 1:2, a.d = c("21", "22")))
  expect_equal(unnestl(dd, s("a/b,d/", stack = T)),
               list(a.b = 1:2, a.d = c("21", "22")))
  expect_equal(unnestl(dd, s("a/b,d/", stack = F)),
               list(a.b = 1L, a.b.2 = 2L, a.d = "21", a.d.2 = "22"))
  expect_equal(unnest(dd, s("a", as = "", s("/", stack = T))),
               dd$a)
  expect_equal(unnestl(dd, s("a//", stack = T)),
               list(a.b = 1:2, a.c = 11:12, a.d = c("21", "22")))
  expect_equal(unnestl(dd, s("a/", stack = "ix")),
               list(a = c("1", "2", "11", "12", "21", "22"),
                    a.ix = c("b", "b", "c", "c", "d", "d")))
  expect_equal(unnest(dd, s("a/", stack = "ix"), stack_atomic = T),
               unnest(dd, s("a/", stack = "ix")))
  expect_equal(unnestl(dd, s("a/", stack = "ix"), stack_atomic = F),
               list(a = c("1", "11", "21"),
                    a.2 = c("2", "12", "22"),
                    a.ix = c("b", "c", "d")))
  expect_equal(unnest(dd, s("a/", stack = "ix"), stack_atomic = F),
               unnest(dd, s("a/", stack = "ix", s(stack = F))))
})

test_that("Stacking atomic vectors works", {

  expect_equal(unnest(y, stack_atomic = T),
               structure(list(a.b.c = c(10, 10, 10, 10, 10, 10, 10, 10, 10,  10, 10, 10),
                              a.b.d = c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L),
                              a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
                              a.b.e.g = c(4L, 5L, 6L, 4L, 5L, 6L, 4L, 5L, 6L, 4L, 5L, 6L)),
                         class = "data.frame", row.names = c(NA, 12L)))

  expect_equal(unnest(y, stack_atomic = T,
                      s("a", s("b", s("e")))),
               structure(list(a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L),
                              a.b.e.g = c(4L, 5L, 6L, 4L, 5L, 6L)),
                         class = "data.frame", row.names = c(NA, 6L)))

  expect_equal(unnest(y, stack_atomic = T,
                      s("a", s("b", s("e", s("g"))))),
               structure(list(a.b.e.g = 4:6), class = "data.frame", row.names = c(NA, 3L)))

  expect_equal(nrow(unnest(y, stack_atomic = T, s("a", s("X")))), 0)
  expect_equal(nrow(unnest(y, s("a", s("X")))), 0)
})

test_that("Exclusion works", {
  expect_equal(unnest(x, stack_atomic = T, s("a", s("b", s(exclude = c("d", "c"))))),
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
  expect_equal(unnestl(x, s("a/b/e", s(2, s(stack = T)))),
               list(a.b.e.g = 4:6))
  expect_equal(unnestl(x, stack_atomic = T, s("a/b/e", s(2))),
               list(a.b.e.g = 4:6))
  expect_equal(unnestl(x, stack_atomic = T, s("a/b/e", s(1), s(2))),
               list(a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L),
                    a.b.e.g = c(4L, 5L, 6L, 4L, 5L, 6L)))
  expect_equal(unnestl(x, stack_atomic = T,
                       s("a/b",
                         s("e", s(2)),
                         s(3, s(3)))),
               list(a.b.c.3.a = c(3, 3, 3),
                    a.b.c.3.b = c(3, 3, 3),
                    a.b.e.g = 4:6))
})

test_that("Multi-chain works", {
  u <- unnestl(x, stack_atomic = T, s("a", s("b/d"), s("b/c/3/a", as = "x")))
  expect_equal(u, l(a.b.d = 2:1, a.x = c(3, 3)))
  expect_equal(u, unnestl(x, stack_atomic = T, s(s("b/d"), s("b/c/3/a", as = "x"))))
  expect_equal(unnestl(x, stack_atomic = T, s("a/b", s("d"), s("c/3/a", as = "x"))),
               l(a.b.d = 2:1, a.b.x = c(3, 3)))
})
