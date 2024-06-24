test_that("zero diagonal etas must have zero off-diagonals (rxode2#481)", {

  expect_error(
    lotri({
      lka <- 0.45
      lcl <- 1
      lvc <- 3.45
      propSd <- fix(0)
      etalcl + etalvc ~ c(0, 0.05, 0)
    }, cov=FALSE),
    NA)

  expect_error(
    lotri({
      lka <- 0.45
      lcl <- 1
      lvc <- 3.45
      propSd <- fix(0)
      etalcl + etalvc ~ c(0, 0.05, 0)
    }, cov=TRUE),
    regexp = "if diagonals are zero, off-diagonals must be zero for covariance matrices (row 1, column 2)",
    fixed = TRUE)

  expect_error(
    lotri({
      lka <- 0.45
      lcl <- 1
      lvc <- 3.45
      propSd <- fix(0)
      etalcl + etalvc ~ c(1, 0.05, 0)
    }, cov=TRUE),
    regexp = "if diagonals are zero, off-diagonals must be zero for covariance matrices (row 2, column 1)",
    fixed = TRUE)

  expect_error(
    lotri({
      lka <- 0.45
      lcl <- 1
      lvc <- 3.45
      propSd <- fix(0)
      etalcl + etalvc ~ c(1, 0.05, 0)
    }, cov=FALSE),
    NA)

  expect_error(
    lotri({
      lka <- 0.45
      lcl <- 1
      lvc <- 3.45
      propSd <- fix(0)
      etalcl + etalvc ~ c(0, 0.05, 1)
    }, cov=TRUE),
    regexp = "if diagonals are zero, off-diagonals must be zero for covariance matrices (row 1, column 2)",
    fixed = TRUE
  )

  expect_error(
    lotri({
      lka <- 0.45
      lcl <- 1
      lvc <- 3.45
      propSd <- fix(0)
      etalcl + etalvc ~ c(0, 0.05, 1)
    }, cov=FALSE),
    NA)

  # call out non-id levels
  expect_error(
    lotri({
      lka <- 0.45
      lcl <- 1
      lvc <- 3.45
      propSd <- fix(0)
      etalcl + etalvc ~ c(0, 0.05, 0) | occ
    }, cov=TRUE),
    regexp="if diagonals are zero, off-diagonals must be zero for covariance matrices (row 1, column 2, level occ)",
    fixed=TRUE
  )


})


test_that("non-positive definite matrix", {

  expect_error(lotri({et1 + et2 ~ c(0.1, 10, 0.1)}, cov=TRUE),
               regexp="non-positive definite matrix covariance matrix",
               fixed=TRUE)

  f <- function(mat) {
    as.matrix(Matrix::nearPD(mat)$mat)
  }

  expect_warning(lotri({et1 + et2 ~ c(0.1, 10, 0.1)}, cov=f),
                 regexp="corrected matrix to be non-positive definite",
                 fixed=TRUE)


  f <- function(mat) {
    mat
  }

  expect_error(lotri({et1 + et2 ~ c(0.1, 10, 0.1)}, cov=f),
               regexp="non-positive definite matrix covariance matrix even after correction",
               fixed=TRUE)

})
