context("Process")

test_that("ASIS processing works", {

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


test_that("PASTE processing works", {

  expect_equal(unnest(xx, s(stack = "x.id",
                            s("a/b/e/", stack = "id", process = "paste"))),
               structure(list(a.b.e = c("1,2", "4,5,6", "1,2", "4,5,6"),
                              a.b.e.id = c("f", "g", "f", "g"),
                              x.id = c(1L, 1L, 2L, 2L)),
                         class = "data.frame", row.names = c(NA, 4L)))

  expect_equal(unnest(xx, s(stack = "x.id",
                            s("a/b/e", s(process = "paste")))),
               structure(list(a.b.e.f = c("1,2", "1,2"),
                              a.b.e.g = c("4,5,6", "4,5,6"),
                              x.id = 1:2),
                         class = "data.frame", row.names = c(NA, 2L)))

  expect_error(unnest(xx, s(stack = "x.id",
                            s("a/b/e", s(process = "blabla")))),
               "Invalid `process` argument")

  expect_equal(unnest(x, s("a/b/e", process = "paste")),
               structure(list(a.b.e = "1:2,4:6"),
                         class = "data.frame", row.names = c(NA, 1L)))

  expect_equal(unnest(xx, s(stack = "x.id",
                            s("a/b/c", process = "paste"))),
               structure(list(a.b.c = c("list(a = 1, b = 1),list(a = 2, c = 2),list(a = 3, b = 3)",
                                        "list(a = 1, b = 1),list(a = 2, c = 2),list(a = 3, b = 3)"),
                              x.id = 1:2),
                         class = "data.frame", row.names = c(NA, 2L)))

  expect_equal(unnest(xx, s(stack = "x.id",
                            s("a/b/c", s(process = "paste")))),
               structure(list(a.b.c.1 = c("1,1", "1,1"),
                              a.b.c.2 = c("2,2", "2,2"),
                              a.b.c.3 = c("3,3", "3,3"),
                              x.id = 1:2),
                         class = "data.frame", row.names = c(NA, 2L)))

  ## ignore excessively nested process spec
  expect_equal(unnest(x, s("a/b/e/", s(process = "asis"))),
               structure(list(a.b.e.f = 1L, a.b.e.f.2 = 2L, a.b.e.g = 4L,
                              a.b.e.g.2 = 5L, a.b.e.g.3 = 6L),
                         class = "data.frame", row.names = c(NA, 1L)))

})

test_that("GC is safe with processing", {

  gctorture(TRUE)
  v1 <- unnest(xx, s(stack = "x.id",
                     s("a/b/e/", stack = "id", process = "paste")))
  v2 <- unnest(xx, s(stack = "x.id",
                     s("a/b/e/", stack = "id", process = "asis")))
  gctorture(FALSE)

  expect_equal(v1,
               structure(list(a.b.e = c("1,2", "4,5,6", "1,2", "4,5,6"),
                              a.b.e.id = c("f", "g", "f", "g"),
                              x.id = c(1L, 1L, 2L, 2L)),
                         class = "data.frame", row.names = c(NA, 4L)))

  expect_equal(v2,
               structure(list(a.b.e = list(1:2, 4:6, 1:2, 4:6),
                              a.b.e.id = c("f", "g", "f", "g"),
                              x.id = c(1L, 1L, 2L, 2L)),
                         class = "data.frame", row.names = c(NA, 4L)))

})
