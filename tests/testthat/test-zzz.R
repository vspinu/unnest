context("GC torture")

# In zzz because it's time consuming and we want these tests at the end.

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
