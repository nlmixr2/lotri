test_that("allNames", {
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

  expect_equal(
    omega$.allNames,
    c(
      "inv.Cl", "inv.Ka", "iov.Cl", "iov.Ka",
      "eye.Cl", "eye.Ka", "eta.Cl", "eta.Ka"
    )
  )

  expect_error(lotriAllNames(1:20))


  mat0 <- lotri(
    eta.Cl ~ 0.1,
    eta.Ka ~ 0.1
  )
  dn <- dimnames(mat0)
  dn0 <- list(dn[[1]], NULL)
  dimnames(mat0) <- dn0

  dn1 <- lotriAllNames(mat0)

  dn0 <- list(NULL, dn[[1]])
  dimnames(mat0) <- dn0

  dn2 <- lotriAllNames(mat0)

  expect_equal(dn1, dn2)

  dn0 <- list(NULL, NULL)
  dimnames(mat0) <- dn0

  dn3 <- lotriAllNames(mat0)

  expect_equal(dn3, character(0))

  dimnames(mat0) <- NULL

  dn3 <- lotriAllNames(mat0)

  expect_equal(dn3, character(0))

  name9 <- c(
    "inv.Cl", "inv.Ka", "iov.Cl", "iov.Ka", "eye.Cl", "eye.Ka",
    "eta.Cl", "eta.Ka"
  )
  expect_equal(lotriAllNames(omega9), name9)
})
