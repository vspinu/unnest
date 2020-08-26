context("Process")

test_that("AsIs processing works", {

  expect_equal(unnest(xx, s(stack = "x.id",
                            s("a/b/e/", stack = "id", process = "asis"))),
               structure(list(a.b.e = list(1:2, 4:6, 1:2, 4:6),
                              a.b.e.id = c("f", "g", "f", "g"),
                              x.id = c(1L, 1L, 2L, 2L)),
                         class = "data.frame", row.names = c(NA, 4L)))

  expect_equal(unnest(xx, s(stack = "x.id",
                            s("a/b/e", s(process = "asis")))),
               structure(list(a.b.e.f = list(1:2, 1:2),
                              a.b.e.g = list(4:6, 4:6),
                              x.id = 1:2),
                         class = "data.frame", row.names = c(NA, 2L)))

  expect_equal(unnest(xx, s(stack = "x.id",
                            s("a/b/e", s(process = "asis")))),
               unnest(xx, s(stack = "x.id",
                            s("a/b/e/", process = "asis"))))

  expect_equal(unnest(x, s("a/b/e", process = "asis")),
               structure(list(a.b.e = list(list(f = 1:2, g = 4:6))),
                         class = "data.frame", row.names = c(NA, 1L)))

  expect_equal(unnest(xx, s(stack = "x.id",
                            s("a/b/e", s("f", process = "asis")))),
               structure(list(a.b.e.f = list(1:2, 1:2),
                              x.id = 1:2),
                         class = "data.frame", row.names = c(NA, 2L)))

  ## ignore excessively nested asis
  expect_equal(unnest(x, s("a/b/e/", s(process = "asis"))),
               structure(list(a.b.e.f = 1L, a.b.e.f.2 = 2L, a.b.e.g = 4L,
                              a.b.e.g.2 = 5L, a.b.e.g.3 = 6L),
                         class = "data.frame", row.names = c(NA, 1L)))

})
