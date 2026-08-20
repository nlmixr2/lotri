.lotri <- loadNamespace("lotri")

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

  ## the variance is 4, so the sd of the prior is 2, and the mean is the
  ## estimate the model already gives
  expect_equal(lotriEst(m)$prior, "dnorm(1, 2)")
  ## it did not become an eta
  expect_equal(dim(m)[1], 0L)
  ## and the estimate is untouched
  expect_equal(lotriEst(m)$est, 1)
})

test_that("the shorthand centers on the estimate, an explicit dnorm does not", {

  ## `tka ~ 4` is the NWPRI pair: the estimate is the prior mean
  ## ($THETAP) and the number on the `~` is its variance ($THETAPV)
  expect_equal(lotriEst(lotri({ tka <- 0.45; tka ~ 4 }))$prior,
               "dnorm(0.45, 2)")

  ## which is exactly what writing that prior out by hand says
  expect_equal(lotriEst(lotri({ tka <- 0.45; prior(tka) ~ dnorm(0.45, 2) }))$prior,
               "dnorm(0.45, 2)")

  ## an explicit prior is taken literally, so it can sit off the estimate
  expect_equal(lotriEst(lotri({ tka <- 0.45; prior(tka) ~ dnorm(0, 2) }))$prior,
               "dnorm(0, 2)")

  ## a zero estimate still renders as the plain zero mean it always was
  expect_equal(lotriEst(lotri({ tka <- 0; tka ~ 4 }))$prior, "dnorm(0, 2)")

  ## the multivariate form carries the whole estimate vector
  expect_equal(lotriEst(lotri({
    tcl <- 3
    tv <- 4
    tcl + tv ~ c(1,
                 0.01, 1)
  }))$prior[1],
  "multiNormal(c(3, 4), lotri(tcl + tv ~ c(1, 0.01, 1)))")

  ## and an all zero mean vector keeps the compact `0` spelling
  expect_equal(lotriEst(lotri({
    tcl <- 0
    tv <- 0
    tcl + tv ~ c(1,
                 0.01, 1)
  }))$prior[1],
  "multiNormal(0, lotri(tcl + tv ~ c(1, 0.01, 1)))")

  ## an `om.` prior is on the omega element, so it is centered on the
  ## omega value the same way a theta one is centered on its estimate
  expect_equal(attr(lotri({
    eta.cl ~ 0.3
    om.eta.cl ~ 0.01
  }), "lotriPriors")[1],
  paste0("dnorm(0.3, ", sqrt(0.01), ")"))
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
               c(NA_character_, "dnorm(3, 1)", "dnorm(4, 1)"))

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

  .expect <- "multiNormal(c(3, 4), lotri(tcl + tv ~ c(1, 0.01, 1)))"
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
               "multiNormal(c(3, 4), lotri(tcl + tv ~ c(4, 0.5, 9)))")

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
  expect_equal(lotriEst(m3)$prior, "dnorm(1, 2)")
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
  expect_equal(lotriEst(.m3)$prior, c("dnorm(1, 2)", "dnorm(2, 3)"))
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

test_that("om. names put a prior on the omega elements", {

  ## a NONMEM TNPRI model puts a normal prior on the omega elements as
  ## well as the thetas, so the elements need names: `om.` prepended to
  ## the between subject variability
  m <- lotri({
    eta.cl ~ 0.3
    eta.v ~ 0.1
    om.eta.cl ~ 0.01
    om.eta.v ~ 0.04
  })

  ## the omega itself is unchanged; only priors were added
  expect_equal(dimnames(m)[[1]], c("eta.cl", "eta.v"))
  expect_equal(as.numeric(diag(m)), c(0.3, 0.1))
  expect_equal(attr(m, "lotriPriors"), c("dnorm(0.3, 0.1)", "dnorm(0.1, 0.2)"))

  ## `prior()` accepts the same names
  m2 <- lotri({
    eta.cl ~ 0.3
    eta.v ~ 0.1
    prior(om.eta.cl) ~ dnorm(0.3, 0.1)
    prior(om.eta.v) ~ dnorm(0.1, 0.2)
  })
  expect_equal(m, m2)

  ## and so does naming the eta directly, since it means the same thing
  m3 <- lotri({
    eta.cl ~ 0.3
    eta.v ~ 0.1
    prior(eta.cl) ~ dnorm(0.3, 0.1)
    prior(eta.v) ~ dnorm(0.1, 0.2)
  })
  expect_equal(m, m3)
})

test_that("om. names support a correlated prior and round trip", {

  m <- lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    om.eta.cl ~ 0.01
    om.eta.v ~ c(0.001, 0.02)
  })

  expect_equal(attr(m, "lotriPriors")[1],
               paste0("multiNormal(c(0.3, 0.1), lotri(om.eta.cl + om.eta.v ~ ",
                      "c(0.01, 0.001, 0.02)))"))
  expect_true(is.na(attr(m, "lotriPriors")[2]))

  expect_equal(as.data.frame(eval(as.expression(m))), as.data.frame(m))
})

test_that("om. names must match a between subject variability", {

  expect_error(lotri({ eta.ka ~ 0.3; om.eta.nope ~ 0.1 }))
  expect_error(lotri({ tka <- 1; om.tka ~ 0.1 }))

  ## an `om.` line does not silently create an eta
  expect_error(lotri({ om.eta.ka ~ 0.1 }))
})

test_that("a NWPRI and a TNPRI omega prior are distinguishable", {

  .nw <- lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    prior(eta.cl, eta.v) ~ invWishart(4)
  })
  .tn <- lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    om.eta.cl ~ 0.01
    om.eta.v ~ c(0.001, 0.02)
  })

  expect_equal(attr(.nw, "lotriPriors")[1], "invWishart(4)")
  expect_true(grepl("^multiNormal", attr(.tn, "lotriPriors")[1]))
})

test_that("~invWishart() sets the degrees of freedom for the whole omega", {

  m <- lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    eta.ka ~ 0.5
    ~invWishart(4)
  })

  ## every block gets it, stored on the first diagonal of each
  expect_equal(attr(m, "lotriPriors"),
               c("invWishart(4)", NA, "invWishart(4)"))

  ## and it round trips, as the equivalent per block priors
  expect_equal(as.data.frame(eval(as.expression(m))), as.data.frame(m))

  ## naming a block as well is a duplicate, not an override
  expect_error(lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    ~invWishart(4)
    prior(eta.cl, eta.v) ~ invWishart(5)
  }))

  ## it needs an omega to apply to
  expect_error(lotri({ tka <- 1; ~invWishart(4) }), "no omega")

  ## the degrees of freedom are still checked against each block
  expect_error(lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    ~invWishart(1)
  }), "degrees of freedom")

  ## a 1x1 block cannot take a correlation matrix prior, even this way
  expect_error(lotri({ eta.ka ~ 0.5; ~lkjCorr(2) }))

  ## a one sided formula that is not a distribution is still an error
  expect_error(lotri(~c(40))) #nolint
})

test_that("an omega cannot have both degrees of freedom and a normal prior", {

  ## NWPRI and TNPRI are alternative ways of putting a prior on the
  ## omega, so a model picks one
  expect_error(lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    eta.ka ~ 0.5
    prior(eta.cl, eta.v) ~ invWishart(4)
    om.eta.ka ~ 0.01
  }), "alternatives")

  ## the same thing said the other way round
  expect_error(lotri({
    eta.cl ~ 0.3
    eta.ka ~ 0.5
    om.eta.cl ~ 0.01
    prior(eta.ka) ~ invWishart(2)
  }))

  ## either one on its own is fine
  expect_error(lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    prior(eta.cl, eta.v) ~ invWishart(4)
  }), NA)
  expect_error(lotri({
    eta.cl ~ 0.3
    eta.v ~ 0.1
    om.eta.cl ~ 0.01
    om.eta.v ~ 0.04
  }), NA)

  ## and a prior that is neither family does not trip the check
  expect_error(lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    eta.ka ~ 0.5
    prior(eta.cl, eta.v) ~ invWishart(4)
    prior(eta.ka) ~ dgamma(2, 1)
  }), NA)
})

test_that("as.expression() renders every prior form correctly", {

  ## This is what `print()` shows and what an estimation method re-parses,
  ## so the exact rendering matters -- a round trip alone could agree with
  ## itself while printing something misleading.

  ## a wide width.cutoff so a long prior line stays one string rather than
  ## a wrapped one the join would pad with extra spaces
  .lines <- function(m) {
    vapply(as.list(as.expression(m)[[2]])[-1],
           function(x) paste(deparse(x, width.cutoff=500L), collapse=" "),
           character(1), USE.NAMES=FALSE)
  }

  expect_true("prior(tka) ~ dnorm(0, 10)" %in%
                .lines(lotri({ tka <- 0.45; prior(tka) ~ dnorm(0, 10) })))

  expect_true("prior(eta.ka) ~ dgamma(2, 1)" %in%
                .lines(lotri({ eta.ka ~ 0.3; prior(eta.ka) ~ dgamma(2, 1) })))

  ## a block prior names the whole block, not just where it is stored
  expect_true("prior(eta.cl, eta.v) ~ lkjCorr(2)" %in%
                .lines(lotri({
                  eta.cl + eta.v ~ c(0.1, 0.01, 0.2)
                  prior(eta.cl, eta.v) ~ lkjCorr(2)
                })))

  expect_true("prior(eta.cl, eta.v) ~ invWishart(4)" %in%
                .lines(lotri({
                  eta.cl + eta.v ~ c(0.1, 0.01, 0.2)
                  prior(eta.cl, eta.v) ~ invWishart(4)
                })))

  ## the whole omega form expands to one line per block
  .wo <- .lines(lotri({
    eta.cl + eta.v ~ c(0.1, 0.01, 0.2)
    eta.ka ~ 0.5
    ~invWishart(4)
  }))
  expect_true("prior(eta.cl, eta.v) ~ invWishart(4)" %in% .wo)
  expect_true("prior(eta.ka) ~ invWishart(4)" %in% .wo)

  ## the normal shorthand prints as the prior it means
  expect_true("prior(tcl) ~ dnorm(3, 1)" %in%
                .lines(lotri({ tcl <- 3; tcl ~ 1 })))

  expect_true("prior(tcl, tv) ~ multiNormal(c(3, 4), lotri(tcl + tv ~ c(1, 0.01, 1)))" %in%
                .lines(lotri({
                  tcl <- 3
                  tv <- 4
                  tcl + tv ~ c(1, 0.01, 1)
                })))

  ## an om. prior prints against the eta it belongs to
  expect_true("prior(eta.cl) ~ dnorm(0.3, 0.1)" %in%
                .lines(lotri({ eta.cl ~ 0.3; om.eta.cl ~ 0.01 })))

  ## whatever spelling went in, the canonical one comes out
  expect_true("prior(eta.cl, eta.v) ~ invWishart(4)" %in%
                .lines(lotri({
                  eta.cl + eta.v ~ c(0.1, 0.01, 0.2)
                  prior(eta.cl, eta.v) ~ inv_wishart(4)
                })))
  expect_true("prior(tka) ~ dnorm(0, 10)" %in%
                .lines(lotri({ tka <- 0.45; prior(tka) ~ normal(0, 10) })))

  ## and every rendering is valid input that gives the same thing back
  for (.m in list(lotri({ tka <- 0.45; prior(tka) ~ dnorm(0, 10) }),
                  lotri({ eta.ka ~ 0.3; prior(eta.ka) ~ dgamma(2, 1) }),
                  lotri({ eta.cl + eta.v ~ c(0.1, 0.01, 0.2)
                          prior(eta.cl, eta.v) ~ invWishart(4) }),
                  lotri({ eta.cl + eta.v ~ c(0.1, 0.01, 0.2)
                          eta.ka ~ 0.5
                          ~invWishart(4) }),
                  lotri({ tcl <- 3; tv <- 4; tcl + tv ~ c(1, 0.01, 1) }),
                  lotri({ eta.cl ~ 0.3; om.eta.cl ~ 0.01 }))) {
    expect_equal(as.data.frame(eval(as.expression(.m))), as.data.frame(.m))
  }
})

test_that("every supported distribution renders and round trips", {

  ## Data driven over the whole table so a new distribution is covered
  ## the moment it is added, and so a wrong `kind`/`nReq` is caught.

  ## a wide width.cutoff so a long prior line stays one string rather than
  ## a wrapped one the join would pad with extra spaces
  .lines <- function(m) {
    vapply(as.list(as.expression(m)[[2]])[-1],
           function(x) paste(deparse(x, width.cutoff=500L), collapse=" "),
           character(1), USE.NAMES=FALSE)
  }

  .d <- lotriPriorDists()
  expect_gt(nrow(.d), 40L)

  for (.i in seq_len(nrow(.d))) {
    .nm <- .d$name[.i]
    ## the required arguments only; anything optional is left off
    .args <- if (.d$nReq[.i] == 0L) "" else paste(rep("2", .d$nReq[.i]), collapse=", ")
    .call <- paste0(.nm, "(", .args, ")")
    ## a matrix or multivariate prior needs a block, everything else a
    ## single unbounded parameter so no support conflicts arise
    if (.d$kind[.i] %in% c("matrix", "multivariate")) {
      .blk <- "eta.a + eta.b ~ c(1, 0.1, 1)"
      .tgt <- "eta.a, eta.b"
    } else {
      .blk <- "tka <- 1"
      .tgt <- "tka"
    }
    .m <- eval(bquote(lotri(.(str2lang(
      paste0("{ ", .blk, "; prior(", .tgt, ") ~ ", .call, " }"))))))

    ## it renders as the prior that was written
    expect_true(paste0("prior(", .tgt, ") ~ ", .call) %in% .lines(.m),
                info=.nm)
    ## and what it renders is valid input giving the same thing back
    expect_equal(as.data.frame(eval(as.expression(.m))), as.data.frame(.m),
                 info=.nm)
  }
})

test_that("every Stan and camelCase spelling normalizes to the canonical one", {

  .d <- lotriPriorDists()

  for (.i in seq_len(nrow(.d))) {
    .args <- if (.d$nReq[.i] == 0L) "" else paste(rep("2", .d$nReq[.i]), collapse=", ")
    if (.d$kind[.i] %in% c("matrix", "multivariate")) {
      .blk <- "eta.a + eta.b ~ c(1, 0.1, 1)"
      .tgt <- "eta.a, eta.b"
    } else {
      .blk <- "tka <- 1"
      .tgt <- "tka"
    }
    .mk <- function(nm) {
      eval(bquote(lotri(.(str2lang(
        paste0("{ ", .blk, "; prior(", .tgt, ") ~ ", nm, "(", .args, ") }"))))))
    }
    ## the canonical, camelCase and Stan spellings are the same prior
    .canon <- .mk(.d$name[.i])
    expect_equal(.mk(.d$camelName[.i]), .canon, info=.d$name[.i])
    expect_equal(.mk(.d$stanName[.i]), .canon, info=.d$stanName[.i])
    if (!is.na(.d$rName[.i])) {
      expect_equal(.mk(.d$rName[.i]), .canon, info=.d$rName[.i])
    }
  }
})

test_that("the distribution name helpers behave", {

  .camel <- .lotri$.lotriSnakeToCamel

  expect_equal(.camel("inv_wishart"), "invWishart")
  expect_equal(.camel("scaled_inv_chi_square"), "scaledInvChiSquare")
  expect_equal(.camel("neg_binomial_2"), "negBinomial2")
  ## a name with no underscore is unchanged
  expect_equal(.camel("gumbel"), "gumbel")
  expect_equal(.camel(c("std_normal", "normal")), c("stdNormal", "normal"))

  .look <- .lotri$.lotriPriorLookup

  ## each of the four spellings finds the same row
  expect_equal(.look("dnorm")$stanName, "normal")
  expect_equal(.look("normal")$stanName, "normal")
  expect_equal(.look("invWishart")$stanName, "inv_wishart")
  expect_equal(.look("inv_wishart")$stanName, "inv_wishart")
  expect_null(.look("notADistribution"))

  .sug <- .lotri$.lotriPriorSuggest

  ## a near miss suggests, something unrecognisable does not
  expect_true(grepl("did you mean", .sug("dnorml")))
  expect_equal(.sug("zzzzzzzzzzqqqqqq"), "")
})

test_that("the prior family classifier separates the omega prior forms", {

  .fam <- .lotri$.lotriPriorFamily

  expect_equal(.fam("invWishart(4)"), "wishart")
  expect_equal(.fam("wishart(4)"), "wishart")
  expect_equal(.fam("dnorm(0, 1)"), "normal")
  expect_equal(.fam("stdNormal()"), "normal")
  expect_equal(.fam("multiNormal(0, 1)"), "normal")
  ## anything else is neither, and so never trips the both check
  expect_equal(.fam("dgamma(2, 1)"), "other")
  expect_equal(.fam("lkjCorr(2)"), "other")
  ## an unparsable or unknown prior is not silently a family
  expect_equal(.fam("this is not a call("), "other")
  expect_equal(.fam("notADistribution(1)"), "other")
  expect_true(is.na(.fam(NA_character_)))
})

test_that("malformed prior calls are rejected", {

  ## a distribution given as a bare name is called with no arguments
  expect_equal(lotriEst(lotri({ tka <- 1; prior(tka) ~ stdNormal }))$prior,
               "stdNormal()")

  ## a bare number is not malformed any more -- it is the normal prior
  ## shorthand, the same as `tka ~ 1` means
  expect_equal(lotriEst(lotri({ tka <- 1; prior(tka) ~ 1 }))$prior,
               lotriEst(lotri({ tka <- 1; tka ~ 1 }))$prior)

  ## a namespaced distribution is refused rather than evaluated as a
  ## variance, which is what the shorthand would otherwise make of it
  ## (reported per line, then re-thrown as the generic "lotri syntax
  ## errors above", so the message itself is not matched here)
  expect_error(lotri({ tka <- 1; prior(tka) ~ stats::dnorm(0, 1) }))

  ## the same argument given twice
  expect_error(lotri({ tka <- 1; prior(tka) ~ dnorm(mean=0, mean=1) }))

  ## a multivariate prior needs more than one parameter
  expect_error(lotri({ tka <- 1; prior(tka) ~ multiNormal(0, 1) }),
               "more than one parameter")

  ## unit support conflicts with a parameter bounded outside [0, 1]
  expect_error(lotri({ tka <- c(0, 1, 10); prior(tka) ~ dbeta(2, 2) }),
               "0, 1")
})

test_that("priors work on conditioned (IOV) models", {

  ## the omega is a list of matrices here rather than one matrix, which
  ## is the path a model with an occasion level takes
  m <- lotri({
    eta.cl ~ 0.3
    iov.cl ~ 0.1 | occ
    prior(eta.cl) ~ dgamma(2, 1)
  })

  expect_true(inherits(m, "lotri") || inherits(m, "list"))
  ## the prior landed on the id level matrix, not the occasion one
  expect_equal(attr(m$id, "lotriPriors"), "dgamma(2, 1)")
  expect_null(attr(m$occ, "lotriPriors"))

  ## and it renders for the list form too
  .l <- vapply(as.list(as.expression(m)[[2]])[-1],
               function(x) paste(deparse(x), collapse=" "), character(1),
               USE.NAMES=FALSE)
  expect_true("prior(eta.cl) ~ dgamma(2, 1)" %in% .l)
})

test_that("labels and priors survive a repeated matrix", {

  ## `list(mat, n)` repeats a block, and the per parameter character
  ## attributes have to be repeated with it
  .m <- lotri({ a + b ~ c(1, 0.1, 1) })
  attr(.m, "lotriLabels") <- c("La", "Lb")
  attr(.m, "lotriPriors") <- c("dgamma(1, 1)", NA_character_)
  class(.m) <- c("lotriFix", class(.m))

  .r <- lotriMat(list(list(.m, 2)), format="ETA[%d]", start=1L)

  expect_equal(dim(.r)[1], 4L)
  expect_equal(attr(.r, "lotriLabels"), c("La", "Lb", "La", "Lb"))
  expect_equal(attr(.r, "lotriPriors"),
               c("dgamma(1, 1)", NA, "dgamma(1, 1)", NA))
})

test_that("the prior parsing helpers handle the odd shapes", {

  .lhs <- .lotri$.lotriTildeLhsNames

  expect_equal(.lhs(quote(a)), "a")
  expect_equal(.lhs(quote(a + b + c)), c("a", "b", "c"))
  ## anything that is not a name or a sum of names has no names
  expect_null(.lhs(quote(f(a))))
  expect_null(.lhs(quote(a + f(b))))
  expect_null(.lhs(quote(1)))

  .whole <- .lotri$.lotriIsWholeOmegaPriorLine

  expect_true(.whole(quote(~invWishart(4))))
  expect_false(.whole(quote(~c(40)))) #nolint
  ## a namespaced call is not a distribution name
  expect_false(.whole(quote(~stats::rnorm(1))))
  expect_false(.whole(quote(~a)))
  expect_false(.whole(quote(a ~ 1)))

  .blk <- .lotri$.lotriBlockIndexes
  .m <- lotri({ a + b ~ c(1, 0.1, 1); d ~ 1 })
  ## starting anywhere in a block finds the whole block, including
  ## extending left from the end of it
  expect_equal(.blk(.m, 1), 1:2)
  expect_equal(.blk(.m, 2), 1:2)
  expect_equal(.blk(.m, 3), 3L)

  .are <- .lotri$.lotriNamesAreBlock
  expect_true(.are(.m, c("a", "b")))
  expect_false(.are(.m, c("a", "d")))
  ## a name that is not in the matrix at all
  expect_false(.are(.m, c("a", "nope")))

  .strip <- .lotri$.lotriStripOm
  expect_equal(.strip(c("om.a", "om.b")), c("a", "b"))
  expect_null(.strip(c("om.a", "b")))
  expect_null(.strip("a"))
  ## `om.` on its own is not a name
  expect_null(.strip("om."))
})

test_that("prior() argument validation", {

  ## a quoted name works the same as a bare one
  expect_equal(lotriEst(lotri({ tka <- 1; prior("tka") ~ dnorm(0, 10) }))$prior,
               "dnorm(0, 10)")

  ## anything that is not a parameter name is rejected.  These are
  ## reported per line and then re-thrown as the generic "lotri syntax
  ## errors above", so the message itself is not matched here
  expect_error(lotri({ tka <- 1; prior(f(tka)) ~ dnorm(0, 10) }))
  expect_error(lotri({ tka <- 1; prior(1) ~ dnorm(0, 10) }))

  ## naming the same parameter twice in one prior
  expect_error(lotri({
    tka <- 1
    tcl <- 2
    prior(tka, tka) ~ multiNormal(0, lotri(tka + tcl ~ c(1, 0.1, 1)))
  }))

  ## and an empty prior()
  expect_error(lotri({ tka <- 1; prior() ~ dnorm(0, 10) }))
})

test_that("deparsing tolerates a prior it cannot parse", {

  ## a hand mangled prior must not stop the object printing; it is
  ## simply not treated as a multivariate one
  m <- lotri({ tka <- 1; tcl <- 2; prior(tka) ~ dnorm(0, 10) })
  .e <- lotriEst(m)
  .e$prior[1] <- "this is not a call("
  attr(m, "lotriEst") <- .e

  ## it warns and leaves that prior out rather than refusing to print
  expect_warning(as.expression(m), "cannot deparse")
  .e <- suppressWarnings(as.expression(m))
  .l <- vapply(as.list(.e[[2]])[-1],
               function(x) paste(deparse(x), collapse=" "), character(1),
               USE.NAMES=FALSE)
  expect_false(any(grepl("prior(", .l, fixed=TRUE)))
  ## the rest of the block is still there
  expect_true("tka <- 1" %in% .l)
})

test_that("two priors reaching the same parameter is an error", {

  ## the same parameter named by two different prior statements, so the
  ## duplicate is only found once they are resolved
  expect_error(lotri({
    tka <- 1
    tcl <- 2
    prior(tka, tcl) ~ multiNormal(0, lotri(tka + tcl ~ c(1, 0.1, 1)))
    prior(tka) ~ dnorm(0, 10)
  }), "more than one prior")

  expect_error(lotri({
    eta.a + eta.b ~ c(1,
                      0.1, 1)
    prior(eta.a, eta.b) ~ lkjCorr(2)
    prior(eta.a) ~ dgamma(1, 1)
  }), "more than one prior")
})

test_that("a prior on a fixed parameter is an error", {

  ## a fixed parameter is a constant, so a prior on it is refused where
  ## the model is defined instead of surfacing downstream in whatever
  ## consumes the priors (nlmixr2/lotri#52)
  expect_error(lotri({
    tka <- fix(1)
    prior(tka) ~ dnorm(1, 1)
  }), "fixed parameter.*'tka'")

  ## the multivariate case from the original report
  expect_error(lotri({
    tcl <- 1
    tv <- fix(3)
    prior(tcl, tv) ~ multiNormal(c(1, 3), lotri(tcl + tv ~ c(1, 0.01, 1)))
  }), "fixed parameter.*'tv'")

  ## a fixed omega element
  expect_error(lotri({
    eta.cl ~ fix(0.1)
    om.eta.cl ~ 0.01
  }), "fixed parameter.*'eta.cl'")

  ## a fixed member of an omega block prior
  expect_error(lotri({
    eta.a + eta.b ~ c(1,
                      0.1, fix(1))
    prior(eta.a, eta.b) ~ lkjCorr(2)
  }), "fixed parameter.*'eta.b'")

  ## a joint theta + om. block naming a fixed omega element
  expect_error(lotri({
    tcl <- 1
    eta.cl ~ fix(0.1)
    prior(tcl, om.eta.cl) ~ multiNormal(c(1, 0.1),
                                        lotri(tcl + om.eta.cl ~ c(1, 0.01, 1)))
  }), "fixed parameter.*'om.eta.cl'")

  ## a joint theta + om. block naming a fixed theta element
  expect_error(lotri({
    tcl <- fix(1)
    eta.cl ~ 0.1
    prior(tcl, om.eta.cl) ~ multiNormal(c(1, 0.1),
                                        lotri(tcl + om.eta.cl ~ c(1, 0.01, 1)))
  }), "fixed parameter.*'tcl'")

  ## a fixed covariance within a block prior, even though every
  ## variance in the block is free
  expect_error(lotri({
    eta.a + eta.b ~ c(1,
                      fix(0.1), 1)
    prior(eta.a, eta.b) ~ lkjCorr(2)
  }), "fixed covariance")

  ## the same, but the block is the omega side of a joint theta + om.
  ## block prior
  expect_error(lotri({
    tcl <- 1
    eta.a + eta.b ~ c(1,
                      fix(0.1), 1)
    prior(tcl, om.eta.a, om.eta.b) ~ multiNormal(c(1, 1, 1),
      lotri(tcl + om.eta.a + om.eta.b ~ c(1, 0.01,1, 0.01,0.01,1)))
  }), "fixed covariance")

  ## the implicit `~invWishart(4)` shorthand applies to every free
  ## block, so it quietly skips one that is entirely fixed instead of
  ## erroring the way an explicit `prior()` on it would
  expect_error(lotri({
    eta.a ~ 0.1
    eta.b ~ fix(0.1)
    ~invWishart(4)
  }), NA)

  ## ... but it still refuses a block that is only partially fixed,
  ## since the joint density it implies cannot hold a constant
  expect_error(lotri({
    eta.a + eta.b ~ c(1,
                      0.1, fix(1))
    ~invWishart(4)
  }), "fixed parameter.*'eta.b'")

  ## ... and a block whose variances are fixed but whose covariance is
  ## still free is partially fixed too, not "entirely fixed" -- it
  ## still has to error rather than being silently skipped
  expect_error(lotri({
    eta.a + eta.b ~ c(fix(1),
                      0.1, fix(1))
    ~invWishart(4)
  }), "fixed parameter")

  ## ... and the other way around: free variances but a fixed
  ## covariance is caught by the shorthand too, not just an explicit
  ## `prior()` on the block
  expect_error(lotri({
    eta.a + eta.b ~ c(1,
                      fix(0.1), 1)
    ~invWishart(4)
  }), "fixed covariance")

  ## `rcm=TRUE` reorders the matrix, but the fix lookup is by name, not
  ## position, so it still finds the right parameter after reordering
  expect_error(lotri({
    eta.a + eta.b + eta.c ~ c(1,
                              0, 1,
                              0.1, 0, fix(1))
    prior(eta.c) ~ dnorm(0, 1)
  }, rcm=TRUE), "fixed parameter.*'eta.c'")

  ## a non-fixed parameter still works
  expect_error(lotri({
    tka <- 1
    prior(tka) ~ dnorm(1, 1)
  }), NA)
})

test_that("the fixed-lookup helpers ignore a name outside the matrix", {

  ## these are only ever called with names drawn from the matrix itself,
  ## but a name that is not among its dimnames is a "not fixed" for all
  ## three helpers rather than an indexing error
  m <- lotri({ eta.a ~ fix(0.1); eta.b ~ 0.2 })

  expect_equal(.lotri$.lotriMatFixedDiag(m, c("eta.a", "nope")),
               c(TRUE, FALSE))
  expect_false(.lotri$.lotriMatFixedCov(m, c("eta.a", "nope")))
  expect_false(.lotri$.lotriMatEntirelyFixed(m, c("eta.a", "nope")))
})

test_that("an older seven column lotriEst still concatenates", {

  ## a `lotriEst` built before the prior column existed has to keep
  ## working when blocks are combined, since the C code reads it
  ## positionally
  .a <- lotri({ tka <- 1; e1 ~ 0.1 })
  .old <- lotriEst(.a)
  .old$prior <- NULL
  expect_equal(length(.old), 7L)
  attr(.a, "lotriEst") <- .old

  .b <- lotri({ tcl <- 2; e2 ~ 0.2 })

  .r <- lotriMat(list(.a, .b))
  .est <- attr(.r, "lotriEst")

  ## the merged frame has the column, filled in as NA for the old block
  expect_true(any(names(.est) == "prior"))
  expect_equal(.est$name, c("tka", "tcl"))
  expect_true(is.na(.est$prior[1]))
})

test_that("printing shows the priors on a matrix", {

  m <- lotri({
    eta.cl + eta.v ~ c(0.1,
                       0.01, 0.2)
    prior(eta.cl, eta.v) ~ invWishart(4)
  })

  .out <- paste(capture.output(print(m)), collapse="\n")
  expect_true(grepl("prior distributions", .out, fixed=TRUE))
  expect_true(grepl("invWishart(4)", .out, fixed=TRUE))
  ## and the attribute is not dumped raw underneath the matrix
  expect_false(grepl("attr(,\"lotriPriors\")", .out, fixed=TRUE))

  ## a model with estimates as well still prints both parts
  m2 <- lotri({
    tka <- 1
    eta.ka ~ 0.3
    prior(tka) ~ dnorm(0, 10)
    prior(eta.ka) ~ dgamma(2, 1)
  })
  .out2 <- paste(capture.output(print(m2)), collapse="\n")
  expect_true(grepl("dnorm(0, 10)", .out2, fixed=TRUE))
  expect_true(grepl("dgamma(2, 1)", .out2, fixed=TRUE))
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

test_that("a joint theta + om. block is one multivariate normal", {

  ## A NONMEM TNPRI model puts the thetas and the omega elements in one
  ## variance matrix, so a prior block may name both.
  m <- lotri({
    tcl <- 1
    eta.cl ~ 0.3
    tcl + om.eta.cl ~ c(0.01,
                        0.002, 0.005)
  })

  .expect <- paste0("multiNormal(c(1, 0.3), ",
                    "lotri(tcl + om.eta.cl ~ c(0.01, 0.002, 0.005)))")

  ## stored once, on the first name of the block, since the block spans
  ## both the estimates and the omega
  expect_equal(lotriEst(m)$prior, .expect)
  expect_null(attr(m, "lotriPriors"))

  ## centered on what the model already says: the estimate for `tcl` and
  ## the omega value for `om.eta.cl`
  expect_equal(lotriEst(m)$est, 1)
  expect_equal(unname(as.matrix(m)[1, 1]), 0.3)

  ## naming the block with prior() means the same thing
  m2 <- lotri({
    tcl <- 1
    eta.cl ~ 0.3
    prior(tcl, om.eta.cl) ~ multiNormal(c(1, 0.3),
                                        lotri(tcl + om.eta.cl ~ c(0.01,
                                                                  0.002, 0.005)))
  })
  expect_equal(lotriEst(m2)$prior, .expect)

  ## and it round trips, which needs the rendered prior() to name every
  ## member of the block rather than only where it is stored
  expect_equal(as.data.frame(eval(as.expression(m))), as.data.frame(m))
  expect_equal(as.data.frame(as.lotri(as.data.frame(m))), as.data.frame(m))
})

test_that("a joint prior is checked like any other", {

  ## an om. name still has to be a real between subject variability
  expect_error(lotri({
    tcl <- 1
    eta.cl ~ 0.3
    tcl + om.eta.nope ~ c(0.01,
                          0.002, 0.005)
  }), "unknown omega element")

  ## degrees of freedom and a normal prior on the omegas stay
  ## alternatives, even when the normal one arrives in a joint block
  expect_error(lotri({
    tcl <- 1
    eta.cl ~ 0.3
    prior(eta.cl) ~ invWishart(4)
    tcl + om.eta.cl ~ c(0.01,
                        0.002, 0.005)
  }), "alternatives, not additions")

  ## a line that is entirely one kind is not joint, and keeps its own
  ## meaning
  expect_equal(lotriEst(lotri({ tcl <- 1; tv <- 2; tcl + tv ~ c(1, 0.1, 1) }))$prior[1],
               "multiNormal(c(1, 2), lotri(tcl + tv ~ c(1, 0.1, 1)))")
  expect_equal(attr(lotri({
    eta.cl + eta.v ~ c(0.3,
                       0.01, 0.1)
    om.eta.cl + om.eta.v ~ c(0.01,
                             0.001, 0.02)
  }), "lotriPriors")[1],
  paste0("multiNormal(c(0.3, 0.1), lotri(om.eta.cl + om.eta.v ~ ",
         "c(0.01, 0.001, 0.02)))"))
})

test_that("a joint block may name omega elements that are not one block", {

  ## A pure `om.` prior has to be exactly one covariance block, because
  ## it is a prior on that block.  A joint block is not -- a TNPRI
  ## variance matrix covers whichever thetas and omega elements it likes
  ## -- so unrelated elements are allowed here.
  m <- lotri({
    tcl <- 1
    eta.cl ~ 0.3
    eta.v ~ 0.1
    tcl + om.eta.cl + om.eta.v ~ c(0.01,
                                   0.002, 0.005,
                                   0.001, 0.0005, 0.004)
  })

  expect_equal(lotriEst(m)$prior,
               paste0("multiNormal(c(1, 0.3, 0.1), lotri(tcl + om.eta.cl + ",
                      "om.eta.v ~ c(0.01, 0.002, 0.005, 0.001, 5e-04, 0.004)))"))
  ## the omega itself is untouched -- only a prior was added
  expect_equal(unname(diag(as.matrix(m))), c(0.3, 0.1))
})

test_that("a joint block may start at the om. name", {

  ## The block is stored on its *first* name, and that name can be either
  ## kind -- so which of the two places it lands in depends on the order
  ## the block was written in.
  m <- lotri({
    tcl <- 1
    eta.cl ~ 0.3
    om.eta.cl + tcl ~ c(0.005,
                        0.002, 0.02)
  })

  .expect <- paste0("multiNormal(c(0.3, 1), ",
                    "lotri(om.eta.cl + tcl ~ c(0.005, 0.002, 0.02)))")

  ## it went to the omega rather than to the estimate table
  expect_equal(attr(m, "lotriPriors")[1], .expect)
  expect_true(is.na(lotriEst(m)$prior))

  ## and it still round trips, since the members come from the covariance
  expect_equal(as.data.frame(eval(as.expression(m))), as.data.frame(m))
})

test_that("a joint block cannot double up on the name it is stored at", {

  ## whichever of the two places the block lands in, something already
  ## there is a duplicate rather than an overwrite
  expect_error(lotri({
    tcl <- 1
    eta.cl ~ 0.3
    tcl ~ 4
    tcl + om.eta.cl ~ c(0.02,
                        0.002, 0.005)
  }), "more than one prior given for 'tcl'")

  expect_error(lotri({
    tcl <- 1
    eta.cl ~ 0.3
    om.eta.cl ~ 0.01
    om.eta.cl + tcl ~ c(0.005,
                        0.002, 0.02)
  }), "more than one prior given for 'om.eta.cl'")
})

test_that("the covariance names come back out of a stored prior", {

  ## how the members of a block are recovered, since a block prior is
  ## stored only once
  expect_equal(.lotri$.lotriPriorCovNames(
    "multiNormal(0, lotri(a + b ~ c(1, 0.1, 1)))"), c("a", "b"))

  ## `lotri({...})` and `lotri(...)` both occur
  expect_equal(.lotri$.lotriPriorCovNames(
    "multiNormal(0, lotri({a + b ~ c(1, 0.1, 1)}))"), c("a", "b"))

  ## anything that is not a covariance carrying prior has no names
  expect_null(.lotri$.lotriPriorCovNames(NA_character_))
  expect_null(.lotri$.lotriPriorCovNames(c("a", "b")))
  expect_null(.lotri$.lotriPriorCovNames("dnorm(0, 1)"))
  expect_null(.lotri$.lotriPriorCovNames("multiNormal(0, lotri(1))"))
  expect_null(.lotri$.lotriPriorCovNames("this is not parseable ("))
})

test_that("a joint block finds its omega in a later nesting level", {

  ## A multi level model has one matrix per level, so the `om.` name is
  ## often not in the first one -- the search has to walk past it.
  m <- lotri({
    tcl <- 1
    eta.cl ~ 0.3 | id
    eta.o ~ 0.1 | occ
    tcl + om.eta.o ~ c(0.02,
                       0.002, 0.005)
  })

  expect_equal(lotriEst(m)$prior,
               paste0("multiNormal(c(1, 0.1), ",
                      "lotri(tcl + om.eta.o ~ c(0.02, 0.002, 0.005)))"))

  ## the omegas themselves are untouched
  expect_equal(as.numeric(m$id), 0.3)
  expect_equal(as.numeric(m$occ), 0.1)

  ## an om. name that is in no level at all is still an error
  expect_error(lotri({
    tcl <- 1
    eta.cl ~ 0.3 | id
    eta.o ~ 0.1 | occ
    tcl + om.eta.nope ~ c(0.02,
                          0.002, 0.005)
  }), "unknown omega element")
})

test_that("prior() can take the normal prior shorthand", {

  ## `prior(tka) ~ 0.1` means what `tka ~ 0.1` means.  The bare form
  ## cannot be used everywhere -- piping onto a model already reads
  ## `tka ~ 0.1` as changing the estimate -- so the `prior()` flag gives
  ## the shorthand a spelling that works there too.
  .p <- function(m) {
    .e <- lotriEst(m); .a <- attr(m, "lotriPriors")
    c(if (!is.null(.e)) .e$prior, if (!is.null(.a)) .a)
  }

  expect_equal(.p(lotri({ tka <- 1; prior(tka) ~ 0.1 })),
               .p(lotri({ tka <- 1; tka ~ 0.1 })))

  ## a correlated group is the same multivariate normal either way
  expect_equal(.p(lotri({ tcl <- 1; tv <- 2; prior(tcl, tv) ~ c(1, 0.01, 1) })),
               .p(lotri({ tcl <- 1; tv <- 2; tcl + tv ~ c(1, 0.01, 1) })))

  ## and an uncorrelated one becomes independent normals, as it does bare
  expect_equal(.p(lotri({ tcl <- 1; tv <- 2; prior(tcl, tv) ~ c(1, 0, 1) })),
               .p(lotri({ tcl <- 1; tv <- 2; tcl + tv ~ c(1, 0, 1) })))

  ## the transformations work here too
  expect_equal(lotriEst(lotri({ tka <- 1; prior(tka) ~ sd(2) }))$prior,
               "dnorm(1, 2)")

  ## an `om.` name puts it on the omega element, as the bare form does
  expect_equal(.p(lotri({ eta.cl ~ 0.3; prior(om.eta.cl) ~ 0.01 })),
               .p(lotri({ eta.cl ~ 0.3; om.eta.cl ~ 0.01 })))

  ## a distribution on the right is still a distribution
  expect_equal(lotriEst(lotri({ tka <- 1; prior(tka) ~ dnorm(0, 10) }))$prior,
               "dnorm(0, 10)")

  ## and the variance is still checked
  expect_error(lotri({ tka <- 1; prior(tka) ~ 0 }))
  expect_error(lotri({ tka <- 1; prior(tka) ~ -1 }))
})

test_that("the two omega spellings agree under the shorthand", {

  ## `prior(eta.cl)` and `prior(om.eta.cl)` are documented as the same
  ## thing, so the shorthand has to center both on the omega value
  .a <- lotri({ eta.cl ~ 0.3; prior(om.eta.cl) ~ 0.01 })
  .b <- lotri({ eta.cl ~ 0.3; prior(eta.cl) ~ 0.01 })
  expect_equal(attr(.a, "lotriPriors"), attr(.b, "lotriPriors"))
  expect_equal(attr(.b, "lotriPriors"), "dnorm(0.3, 0.1)")
})

test_that("prior() shorthand builds a block line by line", {

  ## the per row line form works under `prior()` too, so a covariance
  ## can be built up the same way the bare form builds one
  .p <- function(m) { .e <- lotriEst(m); c(if (!is.null(.e)) .e$prior, attr(m, "lotriPriors")) }

  expect_equal(
    .p(lotri({ tcl <- 3; tv <- 4; prior(tcl) ~ 1; prior(tv) ~ c(0.001, 1) })),
    .p(lotri({ tcl <- 3; tv <- 4; tcl ~ 1; tv ~ c(0.001, 1) })))

  ## and it is the same block the plus form gives
  expect_equal(
    .p(lotri({ tcl <- 3; tv <- 4; prior(tcl) ~ 1; prior(tv) ~ c(0.001, 1) })),
    .p(lotri({ tcl <- 3; tv <- 4; prior(tcl, tv) ~ c(1, 0.001, 1) })))

  ## unrelated single parameters do not get pulled into one block
  expect_equal(
    lotriEst(lotri({ tka <- 1; tcl <- 3; prior(tka) ~ 0.1; prior(tcl) ~ 1 }))$prior,
    c("dnorm(1, 0.316227766016838)", "dnorm(3, 1)"))

  ## a block can be built alongside a scalar prior
  .m <- lotri({
    tka <- 1
    tcl <- 3
    tv <- 4
    prior(tka) ~ 0.1
    prior(tcl) ~ 1
    prior(tv) ~ c(0.001, 1)
  })
  expect_true(grepl("^dnorm", lotriEst(.m)$prior[1]))
  expect_true(all(grepl("^multiNormal", lotriEst(.m)$prior[2:3])))

  ## the om. spelling accumulates the same way
  expect_equal(
    .p(lotri({ eta.cl + eta.v ~ c(0.3, 0.01, 0.1)
               prior(om.eta.cl) ~ 0.01
               prior(om.eta.v) ~ c(0.001, 0.02) })),
    .p(lotri({ eta.cl + eta.v ~ c(0.3, 0.01, 0.1)
               om.eta.cl ~ 0.01
               om.eta.v ~ c(0.001, 0.02) })))
})
