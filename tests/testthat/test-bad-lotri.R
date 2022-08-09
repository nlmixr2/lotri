test_that("bad lower triangular", {
  expect_error(lotri(eta.cl + eta.v + eta.emax + eta.ec50 + eta.kout + eta.e0 ~
                       c(0.0732551, 0, 0.0542082, 0, 0, 0.173761, 0, 0, 0, 0.00741445, 0, 0, 0, 0, 0.00269443)),
               "size do not match")
})
