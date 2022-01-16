test_that("lotriMat", {
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
  
  tmp <- lotriMat(omega9)
  
  expect_error(lotriMatInv(omega9))
  
  tmp2 <- lotriMatInv(tmp)
  
  expect_equal(
    dimnames(tmp)[[1]],
    c(
      "eta.Cl", "eta.Ka", "eye.Cl", "eye.Ka", "iov.Cl",
      "iov.Ka", "inv.Cl", "inv.Ka"
    )
  )
  
  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ fix(1,
              0.5, 1)
  })
  
  unfix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ unfix(1,
                0.5, 1)
  })
  
  
  fix2 <- lotri({
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m+n ~ c(1,
            0.5, 1)
  })
  
  expect_equal(lotriMatInv(lotriEst(lotriMat(list(fix1, fix2)), drop=TRUE)),
               list(structure(c(1, 0.5, 0.5, 1), .Dim = c(2L, 2L),
                              .Dimnames = list(c("f", "g"), c("f", "g")),
                              lotriFix = structure(c(TRUE, TRUE, TRUE, TRUE),
                                                   .Dim = c(2L, 2L),
                                                   .Dimnames = list(c("f", "g"), c("f", "g"))),
                              class = .cls),
                    structure(c(1, 0.5, 0.5, 1), .Dim = c(2L, 2L),
                              .Dimnames = list(c("m", "n"), c("m", "n")),
                              lotriFix = structure(c(FALSE, FALSE, FALSE, FALSE),
                                                   .Dim = c(2L, 2L),
                                                   .Dimnames = list(c("m", "n"), c("m", "n"))),
                              class = .cls)))
  
  expect_equal(vapply(seq_along(tmp2), function(i) {
    dimnames(tmp2[[i]])[[1]]
  }, character(1)), c(
    "eta.Cl", "eta.Ka", "eye.Cl", "eye.Ka", "iov.Cl",
    "iov.Ka", "inv.Cl", "inv.Ka"
  ))
  
  
  
  expect_error(.Call(.lotri$`_asLotriMat`, "a", list(nu = 3), "id", PACKAGE = "lotri"))
  
  expect_error(.Call(.lotri$`_asLotriMat`, matrix(1), list(nu = 3), "id", PACKAGE = "lotri"))
  expect_error(.Call(.lotri$`_asLotriMat`, structure(1, .Dim = c(1L, 1L), dimnames = list(NULL, "a")),
                     list(nu = 3), "id",
                     PACKAGE = "lotri"
  ))
  expect_error(.Call(.lotri$`_asLotriMat`, structure(1, .Dim = c(1L, 1L), dimnames = list("a", NULL)),
                     list(nu = 3), "id",
                     PACKAGE = "lotri"
  ))
  
  expect_error(.Call(.lotri$`_asLotriMat`, lotri(et1 + et2 ~ c(0.1, 0.01, 1)),
                     "a", "id",
                     PACKAGE = "lotri"
  ))
  
  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = 1L, default = c("id", "id2")))
  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = 1L, default = 3))
  
  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), lower = 4))
  
  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    lotri(et5 ~ 6),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2)
  )
  
  expect_equal(lotriMat(testList),
               structure(c(
                 40, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 20, 0.1, 0, 0,
                 0, 0, 0, 0.1, 0.1, 30, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0,
                 0, 0, 0, 0, 0.1, 0.01, 0, 0, 0, 0, 0, 0, 0.01, 1, 0, 0, 0, 0,
                 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
               ), .Dim = c(8L, 8L)))
  
  testList <- list(
    matrix(c(1L, 0L, 0L, 1L), 2, 2),
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    lotri(et5 ~ 6),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1))
  )
  
  expect_equal(lotriMat(testList),
               structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                           0, 40, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 20, 0.1, 0, 0, 0, 0, 0,
                           0.1, 0.1, 30, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0,
                           0, 0.1, 0.01, 0, 0, 0, 0, 0, 0, 0.01, 1), .Dim = c(8L, 8L)))
  
  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    lotri(et5 ~ 6),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
           dimnames = list(
             c("et7", "et8"),
             c("et7", "et8")
           )
    )
  )
  
  expect_equal(
    lotriMat(testList),
    structure(c(
      40, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 20, 0.1, 0, 0,
      0, 0, 0, 0.1, 0.1, 30, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0,
      0, 0, 0, 0, 0.1, 0.01, 0, 0, 0, 0, 0, 0, 0.01, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
    ), .Dim = c(8L, 8L), .Dimnames = list(
      c("et2", "et3", "et4", "et5", "et1", "et6", "et7", "et8"),
      c("et2", "et3", "et4", "et5", "et1", "et6", "et7", "et8")
    ))
  )
  
  expect_error(lotriMat(list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    "A"
  )))
  
  expect_error(lotriMat(3))
  
  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 3),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
           dimnames = list(
             c("et7", "et8"),
             c("et7", "et8")
           )
    )
  )
  
  testList1 <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 3L),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
           dimnames = list(
             c("et7", "et8"),
             c("et7", "et8")
           )
    )
  )
  
  expect_equal(lotriMat(testList), lotriMat(testList1))
  
  expect_error(lotriMat(testList, 4))
  expect_error(lotriMat(testList, "eta[%d]", "a"))
  
  expect_equal(
    dimnames(lotriMat(testList, "ETA[%d]", start = 3))[[1]],
    c(
      "et2", "et3", "et4", "ETA[3]", "ETA[4]", "ETA[5]",
      "et1", "et6", "et7", "et8"
    )
  )
  
  expect_equal(
    dimnames(lotriMat(testList, "ETA[%d]"))[[1]],
    c(
      "et2", "et3", "et4", "ETA[1]", "ETA[2]", "ETA[3]",
      "et1", "et6", "et7", "et8"
    )
  )
  
  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 3, 4),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
           dimnames = list(
             c("et7", "et8"),
             c("et7", "et8")
           )
    )
  )
  expect_error(lotriMat(testList))
  
  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 0),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
           dimnames = list(
             c("et7", "et8"),
             c("et7", "et8")
           )
    )
  )
  expect_error(lotriMat(testList))
  
  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 1:3),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
           dimnames = list(
             c("et7", "et8"),
             c("et7", "et8")
           )
    )
  )
  
  expect_error(lotriMat(testList))
  
  mat1 <- lotri({
    et2 + et3 + et4 ~ c(
      40,
      0.1, 20,
      0.1, 0.1, 30
    )
  })
  
  expect_equal(mat1, lotriMat(mat1))
  
  mat1 <- list(mat1, 3)
  
  expect_equal(lotriMat(mat1), lotriMat(list(mat1)))
  expect_equal(lotriMat(mat1, "ETA[%d]"), lotriMat(list(mat1), "ETA[%d]"))
  expect_equal(lotriMat(mat1, "ETA[%d]", 4), lotriMat(list(mat1), "ETA[%d]", 4L))
  
})
