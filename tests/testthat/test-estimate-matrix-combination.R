test_that("Combined estimates and matrix", {
  
  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
  })
  
  expect_snapshot_output(print(fix1))
  
  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ c(1,
            0.5, 1)
  })
  
  expect_snapshot_output(print(fix1))
  
  expect_equal(attr(fix1, "lotriEst"),
               structure(list(name = c("a", "b", "c", "d", "e"),
                              lower = c(0, 0, -Inf, 0, 0),
                              est = c(1, 1, 1, 1, 1),
                              upper = c(Inf, 2, Inf, 2, 2),
                              fix = c(FALSE, FALSE, TRUE, TRUE, TRUE),
                              label = c("a label",NA, NA, NA, NA),
                              backTransform = c("exp", NA, NA, NA, NA)),
                         row.names = c(NA,-5L), class = "data.frame"))
  fix2 <- fix1
  attr(fix2, "lotriEst") <- NULL
  class(fix2) <- NULL
  
  expect_equal(fix2,
               structure(c(1, 0.5, 0.5, 1),
                         .Dim = c(2L, 2L),
                         .Dimnames = list(c("f", "g"), c("f", "g"))))
  
  fix2 <- lotri({
    a = c(3); label(matt); backTransform(exp) #nolint
  })
  
  expect_equal(attr(fix2, "lotriEst"),
               structure(list(name = "a",
                              lower = -Inf,
                              est = 3,
                              upper = Inf,
                              fix = FALSE,
                              label = "matt",
                              backTransform = "exp"),
                         row.names = c(NA, -1L),
                         class = "data.frame"))
  
  expect_error(expect_message(lotri({a = "matt"})))
  
  expect_error(expect_message(
    lotri({
      a = c(1, 2, 3, 4)
      b <- c(NA) # nolint
      c <- c(NA, NA, NA)
      d <- c(NaN) #nolint
      e <- c(NaN, NaN, NaN)
      f <- Inf
      g <- c(Inf, 1, 2)
      h <- c(0, 1, -Inf)
      i <- c(1, 1, 1)
      j <- c(3, 2, 1)
    })
  ))
  
  # Don't allow dupliate parameters with a mixed matrix/estimate
  expect_error(lotri({b=3;b~0.4}))
})
