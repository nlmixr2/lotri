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
