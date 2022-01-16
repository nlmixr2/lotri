test_that("allNames", {
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
  
  expect_error(.Call(.lotri$`_lotriAllNames`, 1:20, PACKAGE = "lotri"))
  
  
  mat0 <- lotri(
    eta.Cl ~ 0.1,
    eta.Ka ~ 0.1
  )
  dn <- dimnames(mat0)
  dn0 <- list(dn[[1]], NULL)
  dimnames(mat0) <- dn0
  
  dn1 <- .Call(.lotri$`_lotriAllNames`, mat0, PACKAGE = "lotri")
  
  dn0 <- list(NULL, dn[[1]])
  dimnames(mat0) <- dn0
  
  dn2 <- .Call(.lotri$`_lotriAllNames`, mat0, PACKAGE = "lotri")
  
  expect_equal(dn1, dn2)
  
  dn0 <- list(NULL, NULL)
  dimnames(mat0) <- dn0
  
  dn3 <- .Call(.lotri$`_lotriAllNames`, mat0, PACKAGE = "lotri")
  
  expect_equal(dn3, character(0))
  
  dimnames(mat0) <- NULL
  
  dn3 <- .Call(.lotri$`_lotriAllNames`, mat0, PACKAGE = "lotri")
  
  expect_equal(dn3, character(0))
  
  name9 <- c(
    "inv.Cl", "inv.Ka", "iov.Cl", "iov.Ka", "eye.Cl", "eye.Ka",
    "eta.Cl", "eta.Ka"
  )
  expect_equal(
    .Call(.lotri$`_lotriAllNames`, omega9, PACKAGE = "lotri"),
    name9
  )
  
})
