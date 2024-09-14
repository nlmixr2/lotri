test_that("lotri lower triangular matrix specification 2", {

  expect_equal(lotri({
    a ~ c(0.1)
    b ~ c(0.1, 1)
    c ~ c(0.1, 1, 2)
  }),
  lotri({a+b+c ~ c(0.1,
                   0.1, 1,
                   0.1, 1, 2)}))


  expect_equal(lotri({
    a ~ c(a=0.1)
    b ~ c(a=0.1, b=1)
    c ~ c(a=0.1, b=1, c=2)
  }), lotri({a+b+c ~ c(0.1,
                       0.1, 1,
                       0.1, 1, 2)}))



  fix1 <- lotri({
    f+g ~ fix(1,
              0.5, 1) | occ
    m+n ~ c(2,
            0.5, 1)
  })

  fix2 <- lotri({
    f ~ fix(1)
    g ~ fix(0.5, 1) | occ
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    f ~ fix(1) | occ
    g ~ fix(0.5, 1) | occ
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    f ~ fix(1) | occ
    g ~ c(fix(0.5), fix(1))
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    f ~ fix(1) | occ
    g ~ c(fix(0.5), fix(1)) | occ
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix1 <- lotri({
    f + g + h ~ c(fix(1),
                  0.5, 1,
                  0.1, 0.2, 2) | occ
    m + n ~ c(2,
              0.5, 1)
  })

  fix2 <- lotri({
    f ~ fix(1) | occ
    g ~ c(0.5, 1)
    h ~ c(0.1, 0.2, 2)
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    f ~ fix(1)
    g ~ c(0.5, 1) | occ
    h ~ c(0.1, 0.2, 2)
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    f ~ fix(1)
    g ~ c(0.5, 1)
    h ~ c(0.1, 0.2, 2) | occ
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    f ~ fix(1)
    g ~ c(0.5, 1) | occ
    h ~ c(0.1, 0.2, 2) | occ
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    f ~ fix(1) | occ
    g ~ c(0.5, 1)
    h ~ c(0.1, 0.2, 2) | occ
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    f ~ fix(1) | occ
    g ~ c(0.5, 1) | occ
    h ~ c(0.1, 0.2, 2)
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    f ~ fix(1) | occ
    g ~ c(0.5, 1) | occ
    h ~ c(0.1, 0.2, 2) | occ
    m ~ 2
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ fix(1,
              0.5, 1) | occ
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m+n ~ c(1,
            0.5, 1)
  })

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f ~ fix(1) | occ
    g ~ fix(0.5, 1) | occ
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m ~ 1
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f ~ fix(1)
    g ~ fix(0.5, 1) | occ
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m ~ 1
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f ~ fix(1) | occ
    g ~ fix(0.5, 1)
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m ~ 1
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ c(1,
            0.5, 1)
  })

  fix2 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f ~ 1
    g ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m+n ~ c(1,
            0.5, 1)
  })

  fix1 <-  lotri({
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m ~ 1
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  fix2 <- lotri({
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m ~ c(1)
    n ~ c(0.5, 1)
  })

  expect_equal(fix1, fix2)

  test_that("Issue #28", {

    expect_equal(lotri({
      eta1 ~ 0.175278
      eta2 ~ c(0.115896, 0.112362)
      eta3 ~ c(0)
    }),
    lotri(eta1+eta2 ~ c(0.175278, 0.115896, 0.112362),
          eta3 ~  0))

    expect_equal(lotri({
      eta1 ~ 0.175278
      eta2 ~ c(0.115896, 0.112362)
      eta3 ~ c(eta3=0)
    }),
    lotri(eta1+eta2 ~ c(0.175278, 0.115896, 0.112362),
          eta3 ~  0))

    expect_equal(lotri({
      eta1 ~ 0.175278
      eta2 ~ c(0.115896, 0.112362)
      eta3 ~ fix(0)
    }),
    lotri(eta1+eta2 ~ c(0.175278, 0.115896, 0.112362),
          eta3 ~  fix(0)))

    expect_equal(lotri({
      eta1 ~ 0.175278
      eta2 ~ c(0.115896, 0.112362)
      eta3 ~ fix(0)
    }),
    lotri(eta1+eta2 ~ c(0.175278, 0.115896, 0.112362),
          eta3 ~  fix(eta3=0)))

    expect_equal(lotri({
      eta1 ~ 0.175278
      eta2 ~ c(0.115896, 0.112362)
      eta3 ~ 0
    }),
    lotri(eta1+eta2 ~ c(0.175278, 0.115896, 0.112362),
          eta3 ~ 0))

  })

})
