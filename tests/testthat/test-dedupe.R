context("De-duplication")

test_that("Spreading de-duplication works", {

  ref <- unnest(x, s("a", s("b",
                            s("e", s("f/", stack = T)),
                            s("e", s("g/", stack = T)))))

  expect_equal(ref, unnest(x, s("a", s("b",
                                       dedupe = TRUE,
                                       s("e", s("f/", stack = T)),
                                       s("e", s(s(stack = T)))))))

  expect_equal(ref, unnest(x, s("a", dedupe = TRUE,
                                s("b",
                                  s("e/f/", stack = T),
                                  s("e", s(s(stack = T)))))))


  ref <- unnest(x, s("a", dedupe = TRUE,
                     s("b",
                       s("e", s("f")),
                       s("e", s("g")))))

  expect_equal(ref, unnest(x, s("a", dedupe = T,
                                s("b",
                                  s("e", s("f")),
                                  s("e")))))

  expect_equal(ref, unnest(x, s("a", dedupe = FALSE,
                                s("b", dedupe = TRUE,
                                  s("e", s("f")),
                                  s("e")))))

  expect_equal(unnest(x, s("a", dedupe = TRUE,
                           s("b", dedupe = FALSE,
                             s("e", s("f")),
                             s("e")))),
               unnest(x, s("a",
                           s("b",
                             s("e", s("f")),
                             s("e")))))

})

test_that("Deduping with stacking of named level works", {

  y <- l("1" = l(a = 1, b = l(c = 1:2)),
         "2" = l(a = 2, b = l(d = 2L)),
         "3" = l(a = 3, b = l(c = 3L)))

  expect_equal(unnestl(l(x = y), s("x", dedupe = F, s(stack = T, s("a")))),
               list(x.a = c(1, 2, 3)))
  expect_equal(unnestl(l(x = y), s("x", dedupe = T, s(stack = T, s("a")))),
               list(x.a = c(1, 2, 3)))

  expect_equal(unnestl(y, s(stack = T, dedupe = T, s("a"), s("a"), s("b", s(s(stack = T))))),
               list(a = c(1, 1, 2, 3),
                    b.c = c(1L, 2L, NA, 3L),
                    b.d = c(NA, NA, 2L, NA)))

  expect_equal(unnestl(y[1], s(dedupe = T, s("a"), s("b", s("d"),
                                                     s("c", s(stack = T)),
                                                     s("c", as = "C", s(stack = T))))),
               list(`1.a` = c(1, 1, 1, 1),
                    `1.b.C` = c(1L, 2L, 1L, 2L),
                    `1.b.c` = c(1L, 2L, 1L, 2L)))

  # No deduplication due to explicit terminal declaration
  expect_equal(unnestl(y[1], s(dedupe = T, s("a"), s("b", s("d"),
                                                     s("c", s(stack = T)),
                                                     s("c", s(stack = T))))),
               list(`1.a` = c(1, 1, 1, 1),
                    `1.b.c` = c(1L, 2L, 1L, 2L),
                    `1.b.c` = c(1L, 2L, 1L, 2L)))

  # No deduplication due to explicit terminal declaration
  expect_equal(unnestl(y[1], s(dedupe = F, s("a"), s("b", s("d"), s("c"), s("c")))),
               unnestl(y[1], s(dedupe = T, s("a"), s("b", s("d"), s("c"), s("c")))))

  # No deduplication here as deduplication is sequential
  expect_equal(
    unnestl(y, s(dedupe = T, s("a"), s("b", s("d")), s("b"))),
    {
      tt <- unnestl(y, s(dedupe = F, s("a"), s("b", s("d")), s("b")))
      tt[["2.b.d"]] <- NULL
      tt
    })

  expect_equal(unnestl(y, s(stack = T,
                            s("a"),
                            s("b",
                              s("d"),
                              s("c", s(stack = T)),
                              s("c", as = "C", s(stack = T))))),
               list(a = c(1, 1, 1, 1, 2, 3),
                    b.C = c(1L, 2L, 1L, 2L, NA, 3L),
                    b.c = c(1L, 2L, 1L, 2L, NA, 3L),
                    b.d = c(NA, NA, NA, NA, 2L, NA)))

  expect_equal(unnestl(y, s(stack = T, s("a"), s("b", s("d"), s("c", s(stack = T))))),
               unnestl(y, s(stack = T, dedupe = F, s("a"), s("b", s("d"), s("c", s(stack = T))))))

  # NOTE: We are eating duplicated names during stacking. It would not make
  # sense otherwise because during staking one would need to know which
  # variables to join together, leaving only the positional matching as an
  # option.
  expect_equal(unnestl(y, s(stack = T, s("a"), s("b"), s("b", s("d")))),
               unnestl(y, s(stack = T, s("a"), s("b"))))
  expect_equal(unnestl(y, s(stack = T, s("a"), s("b"), s("b", s("d")))),
               unnestl(y, s(stack = T, dedupe = T, s("a"), s("b"), s("b", s("d")))))
  expect_equal(unnestl(y, s(stack = T, s("a"), s("b"), s("b", s("d")))),
               unnestl(y, s(stack = T, dedupe = T, s("a"), s("b"), s("b", s("d")))))

  expect_equal(unnestl(y, s(stack = T,
                            s("a"),
                            s("b",
                              s("d"),
                              s("c", s(stack = T)),
                              s("c", s(stack = T))))),
               list(a = c(1, 1, 1, 1, 2, 3),
                    # b.c = c(1L, 2L, 1L, 2L, NA, 3L),
                    b.c = c(1L, 2L, 1L, 2L, NA, 3L),
                    b.d = c(NA, NA, NA, NA, 2L, NA)))

  expect_equal(unnestl(y, s(stack = T,
                            s("a"),
                            s("b",
                              s("d"),
                              s("c", s(stack = T)),
                              s("c", as = "C", s(stack = T))))),
               list(a = c(1, 1, 1, 1, 2, 3),
                    b.C = c(1L, 2L, 1L, 2L, NA, 3L),
                    b.c = c(1L, 2L, 1L, 2L, NA, 3L),
                    b.d = c(NA, NA, NA, NA, 2L, NA)))

})
