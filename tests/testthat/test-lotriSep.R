test_that("lotriSep", {
  
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
    ) | id(nu = 100),
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye(nu = 50),
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ(nu = 200),
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv(nu = 10)
  )
  
  sep0 <- lotriSep(omega9, above = c(inv = 10L), below = c(eye = 2L, occ = 4L))
  
  attr(sep0$below, "format") <- "ETA[%d]"
  attr(sep0$below, "start") <- 1L
  
  attr(sep0$above, "format") <- "THETA[%d]"
  attr(sep0$above, "start") <- 1L
  
  
  sepA <- lotriSep(omega, above = c(inv = 10L), below = c(eye = 2L, occ = 4L))
  
  sepB <- list(
    above = lotri(lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) |
      inv(nu = 100, same = 10L)),
    below = lotri(
      lotri(
        eta.Cl ~ 0.1,
        eta.Ka ~ 0.1
      ) | id(nu = 100),
      lotri(
        eye.Cl ~ 0.05,
        eye.Ka ~ 0.05
      ) | eye(nu = 50, same = 2L),
      lotri(
        iov.Cl ~ 0.01,
        iov.Ka ~ 0.01
      ) | occ(nu = 200, same = 4L)
    )
  )
  
  attr(sepB$below, "format") <- "ETA[%d]"
  attr(sepB$below, "start") <- 1L
  
  attr(sepB$above, "format") <- "THETA[%d]"
  attr(sepB$above, "start") <- 1L
  
  expect_equal(sepA, sepB)
  
  expect_equal(
    dimnames(lotriMat(sepA$above))[[1]],
    sprintf("THETA[%d]", 1:20)
  )
  expect_equal(
    dimnames(lotriMat(sepA$below))[[1]],
    c("eta.Cl", "eta.Ka", sprintf("ETA[%d]", 1:12))
  )
  
  above1 <- attr(sepA$above, "lotri")
  above1$inv$same <- "matt"
  above <- sepA$above
  attr(above, "lotri") <- above1
  
  expect_equal(dimnames(lotriMat(above))[[1]], c("inv.Cl", "inv.Ka"))
  
  expect_error(lotriSep(omega, above = c(inv = 10L), below = c(eye = 2L, occ = 4L), aboveStart = 1:2))
  
  expect_error(lotriSep(omega, above = c(inv = 10), below = c(eye = 2L, occ = 4L)))
  
  expect_error(lotriSep(omega, above = c(inv = 10L), below = c(eye = 2, occ = 4)))
  
  expect_error(lotriSep(omega, above = 10L, below = c(eye = 2L, occ = 4L)))
  
  expect_error(lotriSep(omega, above = c(inv = 10L), below = c(2L, 4L)))
  
  expect_error(lotriSep(omega, above = c(inv = 10L), below = c(eye = 2L, matt = 4L), aboveStart = 2L))
  
  omega0 <- lotri(
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye(nu = 50),
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ(nu = 200),
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv(nu = 10)
  )
  
  expect_error(lotriSep(omega0, above = c(inv = 10L), below = c(eye = 2L, occ = 4L), aboveStart = 2L))
  
  sepA <- lotriSep(omega, above = NULL, below = c(eye = 2L, occ = 4L))
  
  expect_equal(sepA$above, NULL)
  
  sepA <- lotriSep(omega, above = NULL, below = c(eye = 2L, occ = 4L))
  
  expect_equal(sepA$above, NULL)
  
  ## Bad Lotri matrix
  
  omega1 <- structure(list(id = structure(c(0.1, 0, 0, 0.1), .Dim = c(
    2L,
    2L
  ), .Dimnames = list(c("eta.Cl", "eta.Ka"), c("eta.Cl", "eta.Ka"))), eye = structure(c(0.05, 0, 0, 0.05), .Dim = c(2L, 2L), .Dimnames = list(
    c("eye.Cl", "eye.Ka"), c("eye.Cl", "eye.Ka")
  )), occ = structure(c(
    0.01,
    0, 0, 0.01
  ), .Dim = c(2L, 2L), .Dimnames = list(c("iov.Cl", "iov.Ka"), c("iov.Cl", "iov.Ka"))), inv = structure(c(0.02, 0, 0, 0.02), .Dim = c(2L, 2L), .Dimnames = list(
    c("inv.Cl", "inv.Ka"),
    c("inv.Cl", "inv.Ka")
  ))), lotri = list(
    id = list(nu = 100),
    eye = list(nu = 50), inv = list(nu = 10)
  ), class = "lotri")
  
  expect_error(lotriSep(omega1, above = NULL, below = c(eye = 2L, occ = 4L)))
  
})
