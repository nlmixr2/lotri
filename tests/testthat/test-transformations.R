test_that("transformations", {
  
  expect_equal(lotri(s1 + s2 + s3 ~ cor(sd(1,
                                           0.25, 4,
                                           0.90, 0.50, 9))),
               lotri(s1 + s2 + s3 ~ sd(cor(1,
                                           0.25, 4,
                                           0.90, 0.50, 9))))
  
  expect_equal(lotri(s1 + s2 + s3 ~ cor(sd(1,
                                           0.25, 4,
                                           0.90, 0.50, 9))),
               lotri(s1 + s2 + s3 ~ c(1,
                                      1, 16,
                                      8.1, 18, 81)))
  
  expect_equal(lotri(s1 + s2 + s3 ~ cor(1,
                                        0.25, 4,
                                        0.90, 0.50, 9)),
               lotri(s1 + s2 + s3 ~ c(1,
                                      0.5, 4.0,
                                      2.7, 3.0, 9.0)))
  
  expect_error(lotri(s1 + s2 + s3 ~ cor(sd(1,
                                           2, 4,
                                           0.90, 0.50, 9))))
  
  
  expect_error(lotri(s1 + s2 + s3 ~ sd(var(1,
                                           0.5, 4,
                                           0.90, 0.50, 9))))
  
  expect_equal(diag(lotri(s1 + s2 + s3 ~ var(1,
                                             0.5, 4,
                                             0.90, 0.50, 9))),
               c(s1=1, s2=4, s3=9))
  
  
  ## Cholesky
  m <- matrix(c(2.2,0.4,0,1.6),2,2)
  
  m2 <- m %*% t(m)
  
  m3 <- lotri(s1 + s2 ~ chol(2.2,
                             0.4, 1.6))
  dimnames(m2) <- dimnames(m3)
  
  expect_equal(m2, m3)
  
  
  expect_error(lotri(s1 + s2 ~ sd(chol(2.2,
                                       0.4, 1.6))))
  
  expect_error(lotri(s1 + s2 ~ sd(var(2.2,
                                      0.4, 1.6))))
  
  expect_error(lotri(s1 + s2 ~ cov(cor(2.2,
                                       0.4, 1.6))))
  
  expect_error(lotri(s1 + s2 ~ cor(cov(2.2,
                                       0.4, 1.6))))
  
})

test_that("lotri sd() without cor squares the diagonal", {
  # Single sd value: variance = sd^2
  m <- lotri({ a ~ sd(2) })
  expect_equal(m["a", "a"], 4)

  # 2x2 sd() without cor: diagonal squared, off-diag preserved
  m2 <- lotri({ a + b ~ sd(1, 0.5, 2) })
  expect_equal(m2["a", "a"], 1)  # 1^2 = 1
  expect_equal(m2["b", "b"], 4)  # 2^2 = 4
  expect_equal(m2["a", "b"], 0.5)
})

test_that("lotri unfix() creates a lotriUnfix attribute", {
  m <- lotri({ a + b ~ unfix(1, 0.5, 1) })
  expect_true(!is.null(attr(m, "lotriUnfix")))
  expect_true(inherits(m, "lotriFix"))
})

test_that("lotri var and sd conflict errors", {
  # Error message is wrapped by lotri's {} parser as "lotri syntax errors above"
  expect_error(lotri({ a + b ~ var(sd(1, 0.5, 1)) }))
})

test_that("lotri cor and cov conflict errors", {
  expect_error(lotri({ a + b ~ cov(cor(1, 0.5, 1)) }))
})

test_that("lotri chol with sd errors", {
  # chol used after sd has been set triggers "'chol' has to only be with a single block"
  expect_error(lotri({ a + b ~ sd(chol(1, 0.5, 1)) }))
})

test_that("wrong-size lower triangular triggers an error", {
  expect_error(lotri({ a ~ c(1, 2) }))
  # With multi-name LHS, .lotriMatrix is called and gives specific message
  expect_error(lotri({ a + b ~ c(1, 2) }))
})
