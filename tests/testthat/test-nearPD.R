test_that("lotriNearPD with keepDiag=TRUE preserves diagonal", {
  m <- matrix(c(2, 1.9, 1.9, 2), nrow = 2)
  r <- lotriNearPD(m, keepDiag = TRUE)
  expect_equal(diag(r), diag(m))
})

test_that("lotriNearPD with non-symmetric input triggers ensureSymmetry", {
  m <- matrix(c(2, 1.5, 0.5, 2), nrow = 2)
  expect_false(isSymmetric(m))
  r <- lotriNearPD(m)  # ensureSymmetry=TRUE by default
  expect_equal(dim(r), c(2L, 2L))
  expect_true(isSymmetric(r))
})

test_that("lotriNearPD with trace=TRUE runs without error", {
  m <- matrix(c(1, 0.9, 0.9, 1), nrow = 2)
  # trace uses REprintf (stderr), not R's message/cat system
  expect_no_error(lotriNearPD(m, trace = TRUE))
})

test_that("lotriNearPD on 0x0 matrix returns 0x0 matrix", {
  m0 <- matrix(numeric(0), nrow = 0, ncol = 0)
  r <- lotriNearPD(m0)
  expect_equal(dim(r), c(0L, 0L))
})

test_that("test nearPD with same functions as Matrix", {
  # https://github.com/cran/Matrix/blob/65c37738919156f6bbb682a1d8198d715a82a19f/tests/dpo-test.R#L69-L136
  # Testing nearPD() --- this is partly in  ../man/nearPD.Rd :
  if (requireNamespace("Matrix", quietly = TRUE)) {

    pr <- matrix(c(1,     0.477, 0.644, 0.478, 0.651, 0.826,
                   0.477, 1,     0.516, 0.233, 0.682, 0.75,
                   0.644, 0.516, 1,     0.599, 0.581, 0.742,
                   0.478, 0.233, 0.599, 1,     0.741, 0.8,
                   0.651, 0.682, 0.581, 0.741, 1,     0.798,
                   0.826, 0.75,  0.742, 0.8,   0.798, 1),
                 nrow = 6, ncol = 6)

    pr <- 0.5*(pr + t(pr))

    .npd <- function(x, conv.tol = 1e-7,...) {
      .ret <- as.matrix(Matrix::nearPD(x, conv.tol=conv.tol, ...)$mat)
      dimnames(.ret) <- NULL
      .ret
    }
    expect_equal(lotriNearPD(pr, conv.tol = 1e-7), .npd(pr))
    expect_equal(lotriNearPD(pr, conv.tol = 1e-7, doDykstra=FALSE), .npd(pr, doDykstra=FALSE))

    dimnames(pr) <- list(letters[1:6], letters[1:6])

    pr2 <- lotriNearPD(pr, conv.tol = 1e-7)

    expect_true(identical(dimnames(pr2), dimnames(pr)))

    expect_false(identical(pr2, pr))

  }
})
