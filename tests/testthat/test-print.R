# Tests for print.R: print.lotri, print.lotriFix, str.lotri

test_that("print.lotri shows properties when present", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(lower = 0),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ
  )
  out <- capture.output(print(omega))
  expect_true(length(out) > 0)
  # Properties line appears
  expect_true(any(grepl("Properties", out)))
})

test_that("str.lotri dispatches to .list structure", {
  # Need a lotri with class 'lotri' (requires properties) for str.lotri dispatch
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id,
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ(nu = 100)
  )
  expect_output(str(omega))
})

test_that("print.lotriFix shows 'lotriUnfix' message for unfixed matrix", {
  m <- lotri({ a + b ~ unfix(1, 0.5, 1) })
  expect_true(!is.null(attr(m, "lotriUnfix")))
  out <- capture.output(print(m))
  expect_true(any(grepl("unfix", out, ignore.case = TRUE)))
})

test_that("print.lotriFix with zero-dim and lotriEst only shows estimates", {
  # A lotri with only theta estimates and no eta block creates a 0x0 matrix
  fix0 <- lotri({
    tka <- c(0, 0.45)
    tcl <- c(0, 0.99)
  })
  if (inherits(fix0, "lotriFix") && all(dim(fix0) == 0L)) {
    out <- capture.output(print(fix0))
    expect_true(any(grepl("Estimates", out)))
  }
})
