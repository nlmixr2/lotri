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

.cls <- c("lotriFix", class(matrix(0)))

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

# needs .lotri, omega9
test_that(".maxNu", {

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

  expect_equal(omega$.maxNu, 200)

  expect_equal(.Call(.lotri$`_lotriMaxNu`, omega9, PACKAGE = "lotri"), 0)

})

test_that("isLotri C", {

  expect_equal(.Call(.lotri$`_isLotri`, omega9, PACKAGE = "lotri"), TRUE)
  expect_equal(.Call(.lotri$`_isLotri`, omega, PACKAGE = "lotri"), TRUE)

  omega9[[2]] <- 3
  expect_equal(.Call(.lotri$`_isLotri`, omega9, PACKAGE = "lotri"), FALSE)

  omega9[[2]] <- matrix(3)
  expect_equal(.Call(.lotri$`_isLotri`, omega9, PACKAGE = "lotri"), FALSE)

  expect_equal(.Call(.lotri$`_isLotri`, "matt", PACKAGE = "lotri"), FALSE)

})

test_that("default conditioning", {

  fix2 <- lotri({
    f+g ~ fix(1,
              0.5, 1) | occ
    m+n ~ c(2,
            0.5, 1)
  })

  expect_equal(fix2,
               list(id = structure(c(2, 0.5, 0.5, 1),
                                   .Dim = c(2L, 2L),
                                   .Dimnames = list(c("m", "n"), c("m", "n"))),
                    occ = structure(c(1, 0.5, 0.5, 1),
                                    .Dim = c(2L, 2L),
                                    .Dimnames = list(c("f", "g"), c("f", "g")),
                                    class = .cls,
                                    lotriFix = structure(c(TRUE, TRUE, TRUE, TRUE),
                                                         .Dim = c(2L, 2L),
                                                         .Dimnames = list(c("f", "g"), c("f", "g"))))))

})
