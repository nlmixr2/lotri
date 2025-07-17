test_that("as.data.frame", {

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ fix(1,
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

  fix3 <- lotri({
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m+n ~ c(1,
            0.5, 1); label("n")
  })

  expect_snapshot_output(print(fix3))

  expect_snapshot_output(str(fix3))

  c1 <- lotriMat(list(fix1, fix2))

  expect_error(as.data.frame(c1, row.names=FALSE))

  expect_error(as.data.frame(c1, optional=FALSE))

  c1df <- as.data.frame(c1)

  expect_equal(c1df,
               structure(list(ntheta = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, NA, NA, NA, NA, NA, NA),
                              neta1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 2, 3, 4, 4),
                              neta2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 2, 3, 3, 4),
                              name = c("a", "b", "c", "d", "e", "h", "i", "j", "k", "l", "f", "(f,g)", "g", "m", "(m,n)", "n"),
                              lower = c(0, 0, -Inf, 0, 0, 0, 0, -Inf, 0, 0, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf), est = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 1, 1, 0.5, 1),
                              upper = c(Inf, 2,  Inf, 2, 2, Inf, 2, Inf, 2, 2, Inf, Inf, Inf, Inf, Inf, Inf), fix = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
                              label = c("a label", NA, NA, NA, NA, "b label", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                              backTransform = c("exp", NA, NA, NA, NA, "expit", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                              condition = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "id", "id", "id", "id", "id", "id")),
                         class = "data.frame", row.names = c(NA, -16L)))

  expect_equal(as.lotri(c1df), c1)

  expect_error(as.lotri(c1df[, names(c1df) != "name"]))

  expect_snapshot_output(print(fix1))

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

  expect_snapshot_output(print(fix2))

  df <- as.data.frame(fix2)

  expect_equal(as.data.frame(fix2),
               structure(list(ntheta = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, NA, NA, NA, NA, NA, NA),
                              neta1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 2, 3, 4, 4), neta2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 2, 3, 3, 4),
                              name = c("a", "b", "c", "d", "e", "h", "i", "j", "k", "l", "m", "(m,n)", "n", "f", "(f,g)", "g"),
                              lower = c(0, 0, -Inf, 0, 0, 0, 0, -Inf, 0, 0, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf), est = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 1, 1, 0.5, 1),
                              upper = c(Inf, 2, Inf, 2, 2,  Inf, 2, Inf, 2, 2, Inf, Inf, Inf, Inf, Inf, Inf), fix = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
                              label = c("a label", NA, NA, NA, NA, "b label", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                              backTransform = c("exp", NA, NA, NA, NA, "expit", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                              condition = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "id", "id", "id", "occ", "occ", "occ")),
                         class = "data.frame",
                         row.names = c(NA, -16L)))

  expect_equal(as.lotri(df),
               fix2)

})
