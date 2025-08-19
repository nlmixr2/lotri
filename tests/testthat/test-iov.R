test_that("iov", {

  t <- lotri::lotri({
    tka <- 0.45; label("Ka")
    tcl <- log(2.7); label("Cl")
    tv <- 3.45; label("V")
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    iov.cl ~ 0.1 | occ
    iov.v ~ 0.1 | occ
    eta.v ~ 0.1
    add.sd <- 0.7
  })

  expect_equal(
    t$id,
    lotri::lotri({
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
    })
  )

  expect_equal(
    t$occ,
    lotri::lotri({
      iov.cl ~ 0.1
      iov.v ~ 0.1
    })
  )






})
