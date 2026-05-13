# Additional as.lotri tests not subject to skip_on_cran
# (test-as.lotri.R has skip_on_cran() at file level)

test_that("as.lotri.matrix converts a named matrix to lotri", {
  m <- matrix(c(0.1, 0.05, 0.05, 0.2), nrow = 2, ncol = 2,
               dimnames = list(c("a", "b"), c("a", "b")))
  r <- as.lotri(m)
  expect_true(length(r) > 0)
})

test_that("as.lotri.default converts a plain list to lotri", {
  tmp <- lotri(iov.Ka ~ 0.5, iov.Cl ~ 0.6)
  tmp_plain <- unclass(tmp)
  class(tmp_plain) <- NULL
  r <- as.lotri(tmp_plain)
  expect_true(inherits(r, "lotri"))
})

test_that("as.lotri.default assigns default name to empty-named element", {
  m <- matrix(c(0.1, 0.0, 0.0, 0.2), nrow = 2, ncol = 2,
               dimnames = list(c("a", "b"), c("a", "b")))
  lst <- setNames(list(m), "")
  r <- as.lotri(lst, default = "id")
  expect_equal(names(r), "id")
})

test_that("as.lotri.default errors for unsupported class (lines 106-107)", {
  expect_error(as.lotri("badstring"), "unsupported object")
})
