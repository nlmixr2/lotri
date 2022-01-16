test_that("fixed tests", {
  
  tmp <- lotri(lotri(a + b ~ fix(0.1,
                                 0.001, 0.1)),
               lotri(c + d ~ c(0.1,
                               0.001, 0.1)))
  
  expect_equal(attr(tmp, "lotriFix"),
               structure(c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(4L, 4L), .Dimnames = list(c("a", "b", "c", "d"), c("a", "b", "c", "d"))))
  
  tmp <- lotri(a + b + c ~ c(
    fix(40),
    0.1, 20,
    0.1, 0.1, 30
  ))
  
  expect_equal(attr(tmp, "lotriFix"),
               structure(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(3L, 3L), .Dimnames = list(c("a", "b", "c"),     c("a", "b", "c"))))
  
  tmp <- lotri(a + b + c ~ c(
    40,
    fixed(0.1), 20,
    0.1, 0.1, 30
  ))
  
  expect_equal(attr(tmp, "lotriFix"),
               structure(c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(3L, 3L), .Dimnames = list(c("a", "b", "c"),     c("a", "b", "c"))) )
  
  tmp <- lotri(a + b + c ~ c(
    40,
    0.1, 20,
    fix(0.1), 0.1, 30
  ))
  
  expect_equal(attr(tmp, "lotriFix"),
               structure(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE), .Dim = c(3L, 3L), .Dimnames = list(c("a", "b", "c"),     c("a", "b", "c"))))
  
  expect_snapshot_output(print(tmp))
  
})

