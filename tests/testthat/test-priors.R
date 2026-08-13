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
               c("lkjCorr(2)", NA, "dgamma(2, 1)"))
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
               c("dnorm(0, 10)", "dlnorm(1, 0.5)", "lkjCorr(2)", NA, NA,
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

test_that("a theta on the left of ~ is a normal prior, not an eta", {

  m <- lotri({
    tka <- 1
    tka ~ 4
  })

  ## the variance is 4, so the sd of the prior is 2, and the mean is zero
  expect_equal(lotriEst(m)$prior, "dnorm(0, 2)")
  ## it did not become an eta
  expect_equal(dim(m)[1], 0L)
  ## and the estimate is untouched
  expect_equal(lotriEst(m)$est, 1)
})

test_that("an uncorrelated theta prior block is independent normals", {

  m <- lotri({
    tka <- 1
    tcl <- 3
    tv <- 4
    tcl + tv ~ c(1,
                 0, 1)
  })

  expect_equal(lotriEst(m)$prior,
               c(NA_character_, "dnorm(0, 1)", "dnorm(0, 1)"))

  ## which is the same thing as saying it one row at a time
  m2 <- lotri({
    tka <- 1
    tcl <- 3
    tv <- 4
    tcl ~ 1
    tv ~ c(0, 1)
  })

  expect_equal(m, m2)
})

test_that("a correlated theta prior is a multivariate normal", {

  ## the line (per row) form has to build up the block, exactly like it
  ## does for etas
  m <- lotri({
    tka <- 1
    tcl <- 3
    tv <- 4
    tcl ~ 1
    tv ~ c(0.01, 1)
  })

  .expect <- "multiNormal(0, lotri(tcl + tv ~ c(1, 0.01, 1)))"
  expect_equal(lotriEst(m)$prior, c(NA_character_, .expect, .expect))

  ## and the plus form gives the same thing
  m2 <- lotri({
    tka <- 1
    tcl <- 3
    tv <- 4
    tcl + tv ~ c(1,
                 0.01, 1)
  })

  expect_equal(m, m2)
})

test_that("theta priors accept the matrix transformations", {

  ## sd(2) and sd(3) are variances of 4 and 9
  m <- lotri({
    tcl <- 3
    tv <- 4
    tcl + tv ~ sd(2,
                  0.5, 3)
  })

  expect_equal(lotriEst(m)$prior[1],
               "multiNormal(0, lotri(tcl + tv ~ c(4, 0.5, 9)))")

  ## var() is the default meaning
  m2 <- lotri({
    tcl <- 3
    tv <- 4
    tcl + tv ~ var(4,
                   0.5, 9)
  })

  expect_equal(m, m2)

  ## a single theta with an sd
  m3 <- lotri({ tka <- 1; tka ~ sd(2) })
  expect_equal(lotriEst(m3)$prior, "dnorm(0, 2)")
})

test_that("a theta prior matrix means the same thing as an eta matrix", {

  ## The prior covariance has to be read exactly the way nlmixr2 reads an
  ## eta block -- as a *covariance*, not a correlation -- for every
  ## spelling.  This compares the matrix stored in the prior against the
  ## matrix the identical specification gives for etas.
  .sigma <- function(m) eval(str2lang(lotriEst(m)$prior[1])[[3]])

  .cmp <- function(spec) {
    .eta <- eval(bquote(lotri(.(str2lang(paste0("{ a + b ~ ", spec, " }"))))))
    .th <- eval(bquote(lotri(.(str2lang(
      paste0("{ a <- 1; b <- 2; a + b ~ ", spec, " }"))))))
    expect_equal(unname(as.matrix(.sigma(.th))), unname(as.matrix(.eta)),
                 info=spec)
  }

  .cmp("c(1, 0.5, 2)")
  .cmp("var(1, 0.5, 2)")
  .cmp("cov(1, 0.5, 2)")
  .cmp("sd(1, 0.5, 2)")
  .cmp("sd(cor(1, 0.5, 2))")
  .cmp("chol(1, 0.5, 2)")

  ## an off-diagonal is a covariance, so it is kept as given rather than
  ## being rescaled the way a correlation would be
  .m <- lotri({ a <- 1; b <- 2; a + b ~ c(1, 0.5, 2) })
  expect_equal(.sigma(.m)[1, 2], 0.5)

  ## and cor() really does convert: 0.5 * sd(1) * sd(2) = 1
  .m2 <- lotri({ a <- 1; b <- 2; a + b ~ sd(cor(1, 0.5, 2)) })
  expect_equal(.sigma(.m2)[1, 2], 1)

  ## the uncorrelated case keeps the variance too, as the sd of a dnorm
  .m3 <- lotri({ a <- 1; b <- 2; a + b ~ c(4, 0, 9) })
  expect_equal(lotriEst(.m3)$prior, c("dnorm(0, 2)", "dnorm(0, 3)"))
})

test_that("theta priors round trip", {

  m <- lotri({
    tka <- 1
    tcl <- 3
    tv <- 4
    tka ~ 4
    tcl ~ 1
    tv ~ c(0.01, 1)
  })

  expect_equal(as.data.frame(eval(as.expression(m))), as.data.frame(m))

  ## the multivariate prior comes back as one line naming the group
  expect_true(any(grepl("prior(tcl, tv)", vapply(as.list(as.expression(m)[[2]])[-1],
                                                 deparse1, character(1)),
                        fixed=TRUE)))
})

test_that("estimate only lotri objects can be deparsed", {
  ## regression: this used to fail with "second argument must be a list"
  expect_error(as.expression(lotri({ a <- 1; b <- 2 })), NA)
})

test_that("bad theta priors are errors", {

  ## zero variance is degenerate
  expect_error(lotri({ tka <- 1; tka ~ 0 }), "zero variance")

  ## as is a negative one
  expect_error(lotri({ tka <- 1; tka ~ -1 }))

  ## and a theta cannot have both a shorthand and an explicit prior
  expect_error(lotri({
    tka <- 1
    tka ~ 4
    prior(tka) ~ dnorm(0, 10)
  }))
})

test_that("a name that is not an estimate is still an eta", {

  ## the shorthand must not change how ordinary matrices are parsed
  m <- lotri({
    tka <- 1
    eta.ka ~ 0.6
  })

  expect_equal(dimnames(m)[[1]], "eta.ka")
  expect_true(is.na(lotriEst(m)$prior))
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
  expect_true(all(c("rName", "stanName", "camelName", "name", "parNames",
                    "nPar", "nReq", "support", "kind") %in% names(.d)))
  ## the canonical name is the R one when it is a faithful alias
  expect_equal(.d$name[.d$stanName == "normal"], "dnorm")
  ## and the camelCase one when there is no faithful R equivalent
  expect_equal(.d$name[.d$stanName == "student_t"], "studentT")
  expect_equal(.d$name[.d$stanName == "inv_wishart"], "invWishart")
  expect_equal(.d$name[.d$stanName == "lkj_corr"], "lkjCorr")
  ## a name with no underscore is the same either way
  expect_equal(.d$camelName[.d$stanName == "gumbel"], "gumbel")
  expect_true("lkj_corr" %in% .d$stanName)
  expect_true("inv_wishart" %in% .d$stanName)
  ## every canonical name is distinct
  expect_equal(anyDuplicated(.d$name), 0L)
})

test_that("camelCase and Stan spellings are the same prior", {

  ## camelCase is canonical, the Stan snake_case is an accepted alias
  .camel <- lotri({
    e1 + e2 ~ c(1,
                0.1, 1)
    prior(e1, e2) ~ invWishart(4)
  })
  .snake <- lotri({
    e1 + e2 ~ c(1,
                0.1, 1)
    prior(e1, e2) ~ inv_wishart(4)
  })

  expect_equal(.camel, .snake)
  expect_equal(attr(.camel, "lotriPriors")[1], "invWishart(4)")

  .a <- lotri({ tka <- 1; prior(tka) ~ studentT(3, 0, 10) })
  .b <- lotri({ tka <- 1; prior(tka) ~ student_t(3, 0, 10) })
  expect_equal(.a, .b)
  expect_equal(lotriEst(.a)$prior, "studentT(3, 0, 10)")
})

test_that("omega degrees of freedom can be given on their own", {

  ## the scale matrix is the block itself, so only the degrees of
  ## freedom are needed (the NWPRI $OMEGAPD)
  m <- lotri({
    eta.cl + eta.v ~ c(0.1,
                       0.01, 0.2)
    eta.ka ~ 0.3
    prior(eta.cl, eta.v) ~ invWishart(4)
    prior(eta.ka) ~ invWishart(2)
  })

  expect_equal(attr(m, "lotriPriors"),
               c("invWishart(4)", NA, "invWishart(2)"))

  ## it round trips
  expect_equal(as.data.frame(eval(as.expression(m))), as.data.frame(m))

  ## an explicit scale matrix still works
  expect_error(lotri({
    e1 + e2 ~ c(1,
                0.1, 1)
    prior(e1, e2) ~ invWishart(4, lotri(e1 + e2 ~ c(2,
                                                    0.5, 2)))
  }), NA)

  ## the degrees of freedom are required
  expect_error(lotri({
    e1 + e2 ~ c(1, 0.1, 1)
    prior(e1, e2) ~ invWishart()
  }))

  ## an improper prior is caught, since nu must exceed the dimension - 1
  expect_error(lotri({
    e1 + e2 ~ c(1, 0.1, 1)
    prior(e1, e2) ~ invWishart(1)
  }), "degrees of freedom")

  ## but a 1x1 block only needs nu > 0, since that is an inverse gamma
  expect_error(lotri({ e1 ~ 1; prior(e1) ~ invWishart(1) }), NA)
  expect_error(lotri({ e1 ~ 1; prior(e1) ~ invWishart(0) }))

  ## a correlation matrix prior still needs a real block
  expect_error(lotri({ e1 ~ 1; prior(e1) ~ lkjCorr(2) }))
  ## and a matrix prior cannot go on a population estimate
  expect_error(lotri({ tka <- 1; prior(tka) ~ invWishart(4) }))
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
