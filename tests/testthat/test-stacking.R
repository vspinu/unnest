context("Stacking")

test_that("Stacking of unnamed levels works", {

  expect_equal(unnest(x, s("a", s("b", s("c", s(stack = T))))),
               structure(list(a.b.c.a = c(1, 2, 3),
                              a.b.c.b = c(1, NA, 3),
                              a.b.c.c = c(NA, 2, NA)),
                         class = "data.frame", row.names = c(NA, 3L)))

  expect_equal(unnest(x, s("a", s("b", s("e", s("f/", stack = T)), s("c", s(stack = T))))),
               structure(list(a.b.c.a = c(1, 2, 3, 1, 2, 3),
                              a.b.c.b = c(1, NA, 3, 1, NA, 3),
                              a.b.c.c = c(NA, 2, NA, NA, 2, NA),
                              a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L)),
                         class = "data.frame", row.names = c(NA, 6L)))

  expect_equal(unnest(xx, s(stack = T, s("a", s("b", s("d/", stack = T)), s("id")))),
               structure(list(a.b.d = c(2L, 1L, 2L, 1L),
                              a.id = c(1L, 1L, 2L, 2L)),
                         class = "data.frame", row.names = c(NA, 4L)))

})

test_that("Stacking of named level works", {

  z <- l("1" = l(a = 1, b = l(c = 1:2)),
         "2" = l(a = 2, b = l(d = 2L)),
         "3" = l(a = 3, b = l(c = 3L)))

  expect_equal(unnestl(l(x = z), s("x", s(stack = T, s("a")))),
               list(x.a = c(1, 2, 3)))

  expect_equal(unnestl(z, s(stack = T, s("a"), s("b", s(s(stack = T))))),
               list(a = c(1, 1, 2, 3),
                    b.c = c(1L, 2L, NA, 3L),
                    b.d = c(NA, NA, 2L, NA)))

})

test_that("Double stacking works", {

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

  ## FIXME:
  # expect_equal(unnestl(xx[[1]], s("a", s("id"), s("b", stack = T))),
  #              list(a.b = c(2L, 1L, NA, NA, NA, NA, NA, NA, NA),
  #                   a.b.1.a = c(NA, NA, NA, NA, NA, NA, NA, NA, 1),
  #                   a.b.1.b = c(NA, NA, NA, NA, NA, NA, NA, NA, 1),
  #                   a.b.2.a = c(NA, NA, NA, NA, NA, NA, NA, NA, 2 ),
  #                   a.b.2.c = c(NA, NA, NA, NA, NA, NA, NA, NA, 2),
  #                   a.b.3.a = c(NA, NA, NA, NA, NA, NA, NA, NA, 3),
  #                   a.b.3.b = c(NA, NA, NA, NA, NA, NA, NA, NA, 3),
  #                   a.b.f   = c(NA, NA, 1L, 2L, 1L, 2L, 1L, 2L, NA),
  #                   a.b.g   = c(NA, NA, 4L, 5L, 6L, 4L, 5L, 6L, NA),
  #                   a.id    = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)))

  expect_equal(rbind(unnest(xx[[1]], s("a", s("id"), s("b", s(stack = T)))),
                     unnest(xx[[2]], s("a", s("id"), s("b", s(stack = T))))),
               unnest(xx, s(stack = T, s("a", s("id"), s("b", s(stack = T))))))

})



test_that("Stacking index is created", {

  expect_equal(unnestl(xx, s(stack = I("z.id"),
                            s("a",
                              s("id"),
                              s("b/c", as = "",
                                s(stack = I("c.id")))))),
               list(a.a = c(1, 2, 3, 1, 2, 3),
                    a.b = c(1, NA, 3, 1, NA, 3),
                    a.c = c(NA, 2, NA, NA, 2, NA),
                    a.c.id = c(1L, 2L, 3L, 1L, 2L, 3L),
                    a.id = c(1L, 1L, 1L, 2L, 2L, 2L),
                    z.id = c(1L, 1L, 1L, 2L, 2L, 2L)))

  expect_equal(unnestl(xx, s(stack = I("z.id"),
                             s("a",
                               s("id"),
                               s("b/c", as = "",
                                 s(stack = I("c.id")))))),
               unnestl(xx, s(stack = I("z.id"),
                            s("a",
                              s("id"),
                              s("b/c", as = "",
                                s(stack = I("c.id")))))))
})


test_that("stacking attomics work", {

  expect_equal(unnest(xx, s(stack = T, s("a/b/e/", stack = "id", s(stack = T)))),
               structure(list(a.b.e = c(1L, 2L, 4L, 5L, 6L, 1L, 2L, 4L, 5L, 6L),
                              a.b.e.id = c("f", "f", "g", "g", "g", "f", "f", "g", "g", "g")),
                         class = "data.frame", row.names = c(NA, 10L)))

  ## FIXME: atomic stack ids not working
  # unnest(xx, s(stack = T, s("a/b/e/", as = "e", stack = "var.id",
  #                           s(stack = "el.id"))))

})



test_that("Unnamed stacking works", {


  x <- l(a = l(
           b = l(
             c = list(c = 1, d = 2),
             d = list(1, 2),
             e = l(
               l(a = 1, b = 1),
               l(a = 2, c = 2),
               l(a = 3, b = 3)))))

  expect_equal(unnestl(x, unnamed_lists = "stack"),
               list(a.b.c.c = c(1, 1, 1, 1, 1, 1),
                    a.b.c.d = c(2, 2, 2, 2, 2, 2),
                    a.b.d = c(1, 2, 1, 2, 1, 2),
                    a.b.e.a = c(1, 2, 3, 1, 2, 3),
                    a.b.e.b = c(1, NA, 3, 1, NA, 3),
                    a.b.e.c = c(NA, 2, NA, NA, 2, NA)))

  expect_equal(unnestl(x, unnamed_lists = "exclude"),
               list(a.b.c.c = 1, a.b.c.d = 2))

  expect_equal(unnestl(x, s("a/b/c,d"), unnamed_lists = "exclude"),
               list(a.b.c.c = 1, a.b.c.d = 2, a.b.d.1 = 1, a.b.d.2 = 2))

  expect_equal(unnestl(x, s("a/b", s("c"), s("d/", stack = T)), unnamed_lists = "exclude"),
               list(a.b.c.c = c(1, 1), a.b.c.d = c(2, 2), a.b.d = c(1, 2)))

})
