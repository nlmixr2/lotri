test_that("combine fix1 and fix2", {
  
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
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m+n ~ c(1,
            0.5, 1)
  })
  
  
  c1 <- lotriMat(list(fix1, fix2))
  
  expect_error(lotriMatInv(c1))
  
  expect_equal(attr(c1, "lotriEst"),
               structure(list(name = c("a", "b", "c", "d", "e", "h", "i", "j", "k", "l"),
                              lower = c(0, 0, -Inf, 0, 0, 0, 0, -Inf, 0, 0),
                              est = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                              upper = c(Inf, 2, Inf, 2, 2, Inf, 2, Inf, 2, 2),
                              fix = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
                              label = c("a label", NA, NA, NA, NA, "b label", NA, NA, NA, NA),
                              backTransform = c("exp", NA, NA, NA, NA, "expit", NA, NA, NA, NA)),
                         class = "data.frame", row.names = c(NA, 10L)))
  
  class(c1) <- NULL
  attr(c1, "lotriEst") <- NULL
  
  expect_equal(c1, structure(c(1, 0.5, 0, 0, 0.5, 1, 0, 0, 0, 0, 1, 0.5, 0, 0, 0.5, 1),
                             .Dim = c(4L, 4L),
                             .Dimnames = list(c("f", "g", "m", "n"), c("f", "g", "m", "n"))))
  
  
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
    m+n ~ c(1,
            0.5, 1)
  })
  
  c1 <- lotriMat(list(fix1, fix2))
  
  expect_equal(lotriEst(c1),
               structure(list(name = c("a", "b", "c", "d", "e"),
                              lower = c(0, 0, -Inf, 0, 0),
                              est = c(1, 1, 1, 1, 1),
                              upper = c(Inf, 2, Inf, 2, 2),
                              fix = c(FALSE, FALSE, TRUE, TRUE, TRUE),
                              label = c("a label", NA, NA, NA, NA),
                              backTransform = c("exp", NA, NA, NA, NA)),
                         class = "data.frame", row.names = c(NA, 5L)))
  
  expect_equal(lotriEst(c1, drop=TRUE),
               structure(c(1, 0.5, 0, 0, 0.5, 1, 0, 0, 0, 0, 1, 0.5, 0, 0, 0.5, 1),
                         .Dim = c(4L, 4L),
                         .Dimnames = list(c("f", "g", "m", "n"), c("f", "g", "m", "n"))))
  
  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
  })
  
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
    m+n ~ c(1,
            0.5, 1)
  })
  
  c1 <- lotriMat(list(fix1, fix2))
  
  expect_true(inherits(lotriEst(c1, drop=TRUE), "lotriFix"))
  
})
