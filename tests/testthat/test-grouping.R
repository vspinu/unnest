
test_that("Grouped stacking of unnamed levels works", {

  xx <- l(x, x)
  xx[[1]][["a"]][["id"]] <- 1L
  xx[[2]][["a"]][["id"]] <- 2L

  expect_equal(unnestl(xx, s(stack = T, dedupe = T,
                             groups = list(one = s("a", s("b", s("c"))),
                                           two = s("a", s("b", s("c")))))),
               list(one = list(a.b.c.1.a = c(1, 1), a.b.c.1.b = c(1, 1),
                               a.b.c.2.a = c(2, 2), a.b.c.2.c = c(2, 2),
                               a.b.c.3.a = c(3, 3), a.b.c.3.b = c(3, 3)),
                    two = structure(list(), .Names = character(0))))

  expect_equal(unnestl(xx, s(stack = T, dedupe = T,
                             groups = list(one = s("a", s("b", s("c", s(2)))),
                                           two = s("a", s("b", s("c")))))),
               list(one = list(a.b.c.2.a = c(2, 2), a.b.c.2.c = c(2, 2)),
                    two = list(a.b.c.1.a = c(1, 1), a.b.c.1.b = c(1, 1),
                               a.b.c.3.a = c(3, 3), a.b.c.3.b = c(3, 3))))

  expect_equal(unnest(x, s("a", s("b", s("e", s("f")), s("c", s(stack = T))))),
               structure(list(a.b.c.a = c(1, 2, 3, 1, 2, 3),
                              a.b.c.b = c(1, NA, 3, 1, NA, 3),
                              a.b.c.c = c(NA, 2, NA, NA, 2, NA),
                              a.b.e.f = c(1L, 2L, 1L, 2L, 1L, 2L)),
                         class = "data.frame", row.names = c(NA, 6L)))


  xx <- l(x, x)
  xx[[1]][["a"]][["id"]] <- 1L
  xx[[2]][["a"]][["id"]] <- 2L
  unnest(xx, s(stack = T, dedupe = T, as = ".ix.",
               groups = list(one = list(s("a", s("b", s("d"))),
                                        s("a", s("id"))),
                             two = s("a", s("id"),
                                     s("b", s(exclude = "c"))))))
})


xx <- l(x, x)
xx[[1]][["a"]][["id"]] <- 1L
xx[[2]][["a"]][["id"]] <- 2L
unnest(xx, s(stack = T, dedupe = T, # as = ".ix.",
             groups = list(one = list(s("a", s("b", s("d"))),
                                      s("a", s("id"))),
                           two = s("a",
                                   s("id"),
                                   s("b", s(exclude = "c"))))))

unnest(xx, s(stack = T, dedupe = T, # as = ".ix.",
             groups = list(one = list(s("a", s("b", s("d"))),
                                      s("a", s("id"))),
                           two = s("a"))))

unnest(xx, s(stack = T, dedupe = T, # as = ".ix.",
             groups = list(one = list(s("a", s("b", s("d"))),
                                      s("a", s("id"))),
                           two = list(s("a"),
                                      s("a/id"),
                                      s("a/b/d/[1]")))))


unnest(xx, s(stack = T, dedupe = T,
             s("a", s("b", s(exclude = "c"))),
             s("a/b/d", as = "d2")
             ))

unnest(x, s("a", dedupe = T,
            s("b", s(exclude = "c")),
            s("b")))
