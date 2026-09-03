test_that("attr(x, 'lotri') is read exactly, not by partial matching", {

  ## `lotri` is a prefix of `lotriLabels`/`lotriFix`/..., so an inexact
  ## `attr()` read inside `lotri()` picked up a neighbouring attribute and
  ## then attached it as the condition-property list, giving a bogus
  ## `Properties:` line and a spurious `lotri` class.
  .x <- lotri::lotri(lotri::lotri(b ~ 2) | occ,
                     lotri::lotri({
                       c ~ 1
                       label("z")
                     }))

  expect_null(attr(.x, "lotri", exact = TRUE))
  expect_false(inherits(.x, "lotri"))

  ## genuine properties are unaffected
  .m <- lotri::lotri(lotri::lotri(iov.cl ~ 0.1, iov.v ~ 0.2) |
                       occ(same = 3L, nu = 10))
  expect_equal(attr(.m, "lotri", exact = TRUE)$occ$same, 3L)
  expect_equal(.m$.maxNu, 10)

  .b <- lotri::lotri({
    et1 + et2 ~ c(1, 0.5, 1) | id(lower = 3, upper = 8)
  })
  expect_equal(.b$lower$id, c(et1 = 3, et2 = 3))
  expect_equal(.b$upper$id, c(et1 = 8, et2 = 8))
})
