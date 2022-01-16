test_that("bounds C", {
  .lotri <- loadNamespace("lotri")

  omega9 <- lotri(
    lotri(
      eta.Cl ~ 0.1,
      eta.Ka ~ 0.1
    ) | id,
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye,
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ,
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv
  )
  
  omega <- lotri(
    lotri(
      eta.Cl ~ 0.1,
      eta.Ka ~ 0.1
    ) | id(nu = 100, lower = 3, upper = 4),
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye(nu = 50, lower = c(eye.Cl = 4)),
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ(nu = 200),
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv(nu = 10)
  )

  tmp <- .Call(.lotri$`_lotriGetBounds`, omega9, NULL, NULL, PACKAGE = "lotri")
  expect_true(all(!is.finite(tmp$lower)))
  expect_true(all(!is.finite(tmp$upper)))
  
  expect_equal(omega$upper$id, c(eta.Cl = 4, eta.Ka = 4))
  expect_equal(omega$lower$id, c(eta.Cl = 3, eta.Ka = 3))
  
  lst <- omega$.bounds
  
  expect_equal(lst$lower, c(
    eta.Cl = 3, eta.Ka = 3, eye.Cl = 4, eye.Ka = -Inf, iov.Cl = -Inf,
    iov.Ka = -Inf, inv.Cl = -Inf, inv.Ka = -Inf
  ))
  
  expect_equal(lst$upper, c(
    eta.Cl = 4, eta.Ka = 4, eye.Cl = Inf, eye.Ka = Inf, iov.Cl = Inf,
    iov.Ka = Inf, inv.Cl = Inf, inv.Ka = Inf
  ))
  
  
  omega <- lotri(
    lotri(
      eta.Cl ~ 0.1,
      eta.Ka ~ 0.1
    ) | id(nu = 100, lower = 3, upper = 4),
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye(nu = 50, lower = c(eye.Cl = 4)),
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ(nu = 200),
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv(nu = 10)
  )
  
  sepA <- lotriSep(omega, above = c(inv = 10L), below = c(eye = 2L, occ = 4L))
  
  lst <- sepA$above$.bounds
  expect_equal(names(lst$lower), sprintf("THETA[%d]", 1:20))
  expect_equal(names(lst$upper), sprintf("THETA[%d]", 1:20))
  
  lst <- sepA$below$.bounds
  
  expect_equal(c(
    3, 3, 4, -Inf, 4, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf,
    -Inf, -Inf
  ), as.vector(lst$lower))
  
  expect_equal(c(
    4, 4, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,
    Inf
  ), as.vector(lst$upper))
  
  expect_equal(names(lst$upper), c("eta.Cl", "eta.Ka", sprintf("ETA[%d]", 1:12)))
  
  above <- sepA$above
  
  lotriProp <- attr(above, "lotri")
  lotriProp$inv$same <- 10L
  
  above2 <- above
  attr(above2, "lotri") <- lotriProp
  
  expect_equal(lotriMat(above), lotriMat(above2))
  
  lotriProp$inv$lower <- 3L
  above3 <- above
  attr(above3, "lotri") <- lotriProp
  
  expect_equal(as.vector(above3$.bounds$lower), rep(3.0, 20))
  
  lotriProp$inv$lower <- 3.0
  above4 <- above
  attr(above4, "lotri") <- lotriProp
  
  expect_equal(as.vector(above4$.bounds$lower), rep(3.0, 20))
  
  lotriProp$inv$lower <- c(3, 4)
  above5 <- above
  attr(above5, "lotri") <- lotriProp
  
  expect_error(above5$.bounds)
  
  expect_error(.Call(.lotri$`_lotriGetBounds`, lotri(a ~ 3), NULL, 1, PACKAGE = "lotri"))
  
  lotriProp$inv$lower <- c(inv.Cl = 3L, inv.Ka = 3L)
  above7 <- above
  attr(above7, "lotri") <- lotriProp
  
  
  expect_equal(as.vector(above4$.bounds$lower), rep(3.0, 20))
  
  expect_error(.Call(.lotri$`_lotriGetBounds`, "A", NULL, 1, PACKAGE = "lotri"))
  
})
