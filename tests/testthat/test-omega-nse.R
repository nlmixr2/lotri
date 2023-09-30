test_that("omega as variable works", {

  omega<-c(
    5.08E+00,
    -4.42E-01,  2.31E-01)

  lotri(eta.A+eta.B ~ omega)

  expect_equal(lotri(eta.A+eta.B ~ omega),
               lotri(eta.A+eta.B ~ c(
                 5.08E+00,
                 -4.42E-01,  2.31E-01)))

})
