skip_on_cran()

test_that("as.lotri", {
  tmp2 <- lotri(
    iov.Ka ~ 0.5,
    iov.Cl ~ 0.6
  )

  tmp3 <- as.lotri(tmp2)

  expect_equal(tmp3, structure(list(structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
    c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl")
  ))), .Names = "", class = "lotri"))

  tmp3 <- as.lotri(tmp3, default = "id")

  expect_equal(tmp3, structure(list(id = structure(c(0.5, 0, 0, 0.6), .Dim = c(
    2L,
    2L
  ), .Dimnames = list(c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl")))), class = "lotri"))

  expect_true(inherits(as.matrix(tmp3), "matrix"))

  tmp2 <- lotri(
    lotri(
      iov.Ka ~ 0.5,
      iov.Cl ~ 0.6
    ) | iov(lower = 3),
    lotri(
      occ.Ka ~ 0.5,
      occ.Cl ~ 0.6
    )
  )

  expect_error(as.matrix(tmp2))

  l1 <- as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), lower = 4, default = "id")

  l2 <- lotri(et1 + et2 ~ c(0.1, 0.01, 1) | id(lower = 4))

  expect_equal(l1, l2)

  l1 <- as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), nu = 4, default = "id")
  l2 <- lotri(et1 + et2 ~ c(0.1, 0.01, 1) | id(nu = 4))

  expect_equal(l1, l2)

  l1 <- as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = c(et1 = 3), default = "id")
  l2 <- lotri(et1 + et2 ~ c(0.1, 0.01, 1) | id(upper = c(et1 = 3)))

  expect_equal(l1, l2)


  l1 <- as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = c(et1 = 3), matt = NULL, default = "id")
  l2 <- lotri(et1 + et2 ~ c(0.1, 0.01, 1) | id(upper = c(et1 = 3)))

  expect_equal(l1, l2)

  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = c(3, 3), default = "id"))
  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = 1L, default = "id"))

  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), lower = c(3, 3), default = "id"))
  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), lower = 1L, default = "id"))

  expect_error(as.lotri("matt"))
})
