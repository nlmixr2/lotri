# Tests for R/lotriMatInv.R: lotriMatInv, lotriIsBlockMat

test_that("lotriMatInv errors on non-matrix input (line 63)", {
  expect_error(lotriMatInv("not_a_matrix"), "'mat' must be a matrix")
})

test_that("lotriMatInv errors on matrix with lotriEst attribute (lines 65-67)", {
  m <- lotri({
    a <- c(0, 1)
    b <- c(0, 1)
  })
  expect_error(lotriMatInv(m), "lotri matrix with attached estimates")
})

test_that("lotriIsBlockMat returns TRUE for 1x1 block (line 172)", {
  # A diagonal 2x2 matrix has two 1x1 blocks; the vapply hits dim==1 path
  m <- lotri({ a ~ 1; b ~ 1 })
  result <- lotriIsBlockMat(m)
  expect_true(result)
})
