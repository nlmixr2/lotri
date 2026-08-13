test_that("priors can be given on population estimates", {

  m <- lotri({
    tka <- 0.45
    tcl <- c(0, 1, 10)
    prior(tka) ~ dnorm(0, 10)
    prior(tcl) ~ dlnorm(1, 0.5)
  })

  expect_equal(lotriEst(m)$prior, c("dnorm(0, 10)", "dlnorm(1, 0.5)"))

  ## the Stan spelling is accepted and normalized to the canonical R name
  m2 <- lotri({
    tka <- 0.45
    tcl <- c(0, 1, 10)
    prior(tka) ~ normal(0, 10)
    prior(tcl) ~ lognormal(1, 0.5)
  })

  expect_equal(m, m2)

  ## named arguments, in any order, give the same thing
  m3 <- lotri({
    tka <- 0.45
    tcl <- c(0, 1, 10)
    prior(tka) ~ dnorm(sd=10, mean=0)
    prior(tcl) ~ dlnorm(meanlog=1, sdlog=0.5)
  })

  expect_equal(m, m3)
})

test_that("priors can be given on etas and on covariance blocks", {

  m <- lotri({
    eta.cl + eta.v ~ c(0.1,
                       0.01, 0.2)
    eta.ka ~ 0.3
    prior(eta.ka) ~ dgamma(2, 1)
    prior(eta.cl, eta.v) ~ lkj_corr(2)
  })

  ## the block prior is stored on the first diagonal of the block
  expect_equal(attr(m, "lotriPriors"),
               c("lkj_corr(2)", NA, "dgamma(2, 1)"))
})

test_that("prior lines are order independent", {

  m1 <- lotri({
    tka <- 0.45
    prior(tka) ~ dnorm(0, 10)
  })

  m2 <- lotri({
    prior(tka) ~ dnorm(0, 10)
    tka <- 0.45
  })

  expect_equal(m1, m2)
})

test_that("a prior line does not steal a label from the previous estimate", {

  m <- lotri({
    tka <- 0.45
    prior(tka) ~ dnorm(0, 10)
    label("Ka")
  })

  expect_equal(lotriEst(m)$label, "Ka")
  expect_equal(lotriEst(m)$prior, "dnorm(0, 10)")
})

test_that("a prior does not leak an estimate from its arguments", {

  ## `mean <- 0` inside the distribution used to be picked up as a theta
  m <- lotri({
    tka <- 0.45
    prior(tka) ~ dnorm(mean <- 0, sd = 10)
  })

  expect_equal(lotriEst(m)$name, "tka")
})

test_that("priors round trip through as.expression() and as.data.frame()", {

  m <- lotri({
    tka <- 0.45
    label("Ka")
    tcl <- c(0, 1, 10)
    eta.cl + eta.v ~ c(0.1,
                       0.01, 0.2)
    eta.ka ~ 0.3
    prior(tka) ~ dnorm(0, 10)
    prior(tcl) ~ dlnorm(1, 0.5)
    prior(eta.ka) ~ dgamma(2, 1)
    prior(eta.cl, eta.v) ~ lkj_corr(2)
  })

  expect_equal(as.data.frame(eval(as.expression(m))), as.data.frame(m))

  .df <- as.data.frame(m)
  expect_equal(.df$prior,
               c("dnorm(0, 10)", "dlnorm(1, 0.5)", "lkj_corr(2)", NA, NA,
                 "dgamma(2, 1)"))

  ## and back from the data frame
  expect_equal(as.data.frame(as.lotri(.df))$prior, .df$prior)
})

test_that("as.lotri() still works on a data.frame without a prior column", {

  m <- lotri({
    tka <- 0.45
    eta.ka ~ 0.3
  })

  .df <- as.data.frame(m)
  .df <- .df[, names(.df) != "prior"]

  expect_error(as.lotri(.df), NA)
})

test_that("a zero row estimate frame still converts", {

  ## a model with no population estimates round trips through a data
  ## frame whose theta part has no rows at all
  m <- lotri({ eta.ka ~ 0.3; label("KA") })

  .df <- as.data.frame(m)
  .l <- as.lotri(.df)

  expect_error(as.data.frame(.l), NA)
  expect_equal(nrow(as.data.frame(.l)), 1L)
  expect_true(is.na(as.data.frame(.l)$prior))
})

test_that("priors survive combining lotri matrices", {

  a <- lotri({ e1 ~ 0.1 })
  b <- lotri({
    e2 ~ 0.2
    prior(e2) ~ dgamma(1, 1)
  })

  ab <- lotri(a, b)

  expect_equal(dimnames(ab)[[1]], c("e1", "e2"))
  expect_equal(attr(ab, "lotriPriors"), c(NA, "dgamma(1, 1)"))
})

test_that("priors are matched by name so rcm re-ordering is safe", {

  m <- lotri({
    a ~ 1
    b ~ c(0, 1)
    c ~ c(0.5, 0, 1)
    prior(a) ~ dgamma(1, 1)
  }, rcm=TRUE)

  .w <- which(dimnames(m)[[1]] == "a")
  expect_equal(attr(m, "lotriPriors")[.w], "dgamma(1, 1)")
})

test_that("bad priors are errors", {

  ## unknown distribution
  expect_error(lotri({ a <- 1; prior(a) ~ dnorml(0, 1) }))

  ## wrong number of arguments
  expect_error(lotri({ a <- 1; prior(a) ~ dnorm(0) }))
  expect_error(lotri({ a <- 1; prior(a) ~ dnorm(0, 1, 2) }))

  ## unknown argument name
  expect_error(lotri({ a <- 1; prior(a) ~ dnorm(mu=0, sd=1) }))

  ## `dt` is not `student_t`
  expect_error(lotri({ a <- 1; prior(a) ~ dt(3) }))

  ## unknown parameter
  expect_error(lotri({ a <- 1; prior(b) ~ dnorm(0, 1) }))

  ## duplicated prior
  expect_error(lotri({
    a <- 1
    prior(a) ~ dnorm(0, 1)
    prior(a) ~ dnorm(0, 2)
  }))

  ## a matrix prior needs a block
  expect_error(lotri({ a <- 1; prior(a) ~ lkj_corr(2) }))

  ## a univariate prior cannot cover a block
  expect_error(lotri({
    e1 + e2 ~ c(1, 0.5, 1)
    prior(e1, e2) ~ dnorm(0, 1)
  }))

  ## the names have to be a single block
  expect_error(lotri({
    e1 ~ 1
    e2 ~ 1
    prior(e1, e2) ~ lkj_corr(2)
  }))

  ## positive support conflicts with a negative lower bound
  expect_error(lotri({
    a <- c(-10, 1, 10)
    prior(a) ~ dlnorm(0, 1)
  }))

  ## empty prior()
  expect_error(lotri({ a <- 1; prior() ~ dnorm(0, 1) }))
})

test_that("lotriPriorDists() describes the supported distributions", {

  .d <- lotriPriorDists()

  expect_true(inherits(.d, "data.frame"))
  expect_true(all(c("rName", "stanName", "name", "parNames", "nPar",
                    "support", "kind") %in% names(.d)))
  ## the canonical name is the R one when it is a faithful alias
  expect_equal(.d$name[.d$stanName == "normal"], "dnorm")
  ## and the Stan one when there is no faithful R equivalent
  expect_equal(.d$name[.d$stanName == "student_t"], "student_t")
  expect_true("lkj_corr" %in% .d$stanName)
  expect_true("inv_wishart" %in% .d$stanName)
})

test_that("labels follow the matrix when rcm re-orders it", {

  ## regression: the labels used to stay in parse order while the
  ## matrix was permuted, so they ended up on the wrong parameters
  m <- lotri({
    a ~ 1
    label("A lbl")
    b ~ c(0, 1)
    label("B lbl")
    c ~ c(0.5, 0, 1)
    label("C lbl")
  }, rcm=TRUE)

  .lab <- attr(m, "lotriLabels")
  .nm <- dimnames(m)[[1]]

  expect_equal(.lab[.nm == "a"], "A lbl")
  expect_equal(.lab[.nm == "b"], "B lbl")
  expect_equal(.lab[.nm == "c"], "C lbl")
})

test_that("labels survive combining lotri matrices", {

  ## regression: `lotriLabels` used to be dropped by the C concatenation
  a <- lotri({ e1 ~ 0.1; label("L1") })
  b <- lotri({ e2 ~ 0.2; label("L2") })

  ab <- lotri(a, b)

  expect_equal(attr(ab, "lotriLabels"), c("L1", "L2"))
})
