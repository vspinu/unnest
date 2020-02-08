context("Naming")

test_that("Renaming works", {
  expect_equal(names(unnest(x, s("a", as = "A", s("b", s("e", as = "E", s("g")))))),
               "A.b.E.g")
  out <- unnest(x, s("a",
                     s("b",
                       s("e", as = "E", s("f", as = "F")),
                       s("c", as = "C", s(stack = T)))))
  expect_equal(names(out),  c("a.b.C.a", "a.b.C.b", "a.b.C.c", "a.b.E.F"))
  expect_equal(unnest(x, s("a", s("b", as = "", s("e", as = "E", s("g"))))),
               structure(list(a.E.g = 4:6), class = "data.frame", row.names = c(NA, 3L)))
})

test_that("Dropping names works", {
  expect_equal(unnestl(x, s("a/b", s("c/[2]/a", as = "x"))),
               list(a.b.x = 2))

  expect_equal(unnestl(x, s("a/b/c/[2]/a", as = "")),
               structure(list(2), .Names = ""))

  expect_equal(unnestl(x, s("a/b/c/[2]", as = "")),
               list(a = 2, c = 2))
})

test_that("Short form node spec works", {
  expect_equal(unnestl(x, s("a/b/d", as = "aaa")),
               list(aaa = 2:1))
  expect_equal(unnestl(x, s("a//d")),
               list(a.b.d = 2:1))
  expect_equal(unnestl(x, s("a//c/[3]")),
               list(a.b.c.3.a = 3, a.b.c.3.b = 3))
  expect_equal(unnestl(x, s("a//[3]/[3]")),
               list(a.b.c.3.a = 3, a.b.c.3.b = 3))
  expect_equal(unnestl(x, s("[1]/[1]/[3]/[3]")),
               list(a.b.c.3.a = 3, a.b.c.3.b = 3))
})


test_that("Head and tail empty spec works", {
  expect_equal(unnestl(x, s("/b/d", as = "aaa")),
               list(a.aaa = 2:1))
  expect_equal(unnestl(x, s("a/b/c/")),
               unnestl(x, s("a/b/c//")))
  expect_equal(unnestl(x, s("a/b/c/", stack = T)),
               list(a.b.c.a = c(1, 2, 3),
                    a.b.c.b = c(1, NA, 3),
                    a.b.c.c = c(NA, 2, NA)))
})
