test_that("a marginal normal prior on an off-diagonal omega covariance element parses", {
  m <- lotri({
    eta.cl + eta.v ~ c(0.3, 0.05, 0.2)
    prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
  })
  expect_equal(attr(m, "lotriOffDiagPriors"),
               c("(eta.cl,eta.v)" = "dnorm(0, 0.1)"))
  ## no diagonal prior was ever set
  expect_null(attr(m, "lotriPriors"))
})

test_that("the om.<eta> spelling of a covariance-pair prior is identical to the bare spelling", {
  m1 <- lotri({
    eta.cl + eta.v ~ c(0.3, 0.05, 0.2)
    prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
  })
  m2 <- lotri({
    eta.cl + eta.v ~ c(0.3, 0.05, 0.2)
    prior(om.eta.cl, om.eta.v) ~ dnorm(0, 0.1)
  })
  expect_identical(attr(m1, "lotriOffDiagPriors"), attr(m2, "lotriOffDiagPriors"))
})

test_that("a covariance-pair prior on names that do not covary is refused", {
  expect_error(
    lotri({
      eta.cl ~ 0.3
      eta.v ~ 0.2
      prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
    }),
    "not a single covariance block")
})

test_that("a covariance-pair prior on a SUBSET of a larger correlated block succeeds", {
  ## this is the crux of the new capability: .lotriNamesAreBlock() would
  ## refuse {eta.cl, eta.v} here because it is not the WHOLE (3-eta) block;
  ## the relaxed .lotriNamesAreCovPair() only requires the two to covary
  m <- lotri({
    eta.ka + eta.cl + eta.v ~ c(0.6, 0.01, 0.3, 0.02, 0.03, 0.2)
    prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
  })
  expect_equal(attr(m, "lotriOffDiagPriors"),
               c("(eta.cl,eta.v)" = "dnorm(0, 0.1)"))
})

test_that("a whole-block distribution still requires the ENTIRE block, unaffected by the relaxation", {
  expect_error(
    lotri({
      eta.ka + eta.cl + eta.v ~ c(0.6, 0.01, 0.3, 0.02, 0.03, 0.2)
      prior(eta.cl, eta.v) ~ invWishart(4)
    }),
    "not a single covariance block")
})

test_that("a block cannot carry both a whole-block invWishart() and a marginal normal on one of its cells", {
  expect_error(
    lotri({
      eta.ka + eta.cl + eta.v ~ c(0.6, 0.01, 0.3, 0.02, 0.03, 0.2)
      prior(eta.ka, eta.cl, eta.v) ~ invWishart(4)
      prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
    }),
    "cannot have both degrees of freedom")
})

test_that("a covariance-pair prior round-trips through as.data.frame()/as.lotri()/as.expression()", {
  m <- lotri({
    eta.cl + eta.v ~ c(0.3, 0.05, 0.2)
    prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
  })
  df <- as.data.frame(m)
  .w <- which(df$name == "(eta.cl,eta.v)")
  expect_length(.w, 1L)
  expect_equal(df$prior[.w], "dnorm(0, 0.1)")
  expect_true(is.na(df$prior[df$name == "eta.cl"]))
  expect_true(is.na(df$prior[df$name == "eta.v"]))

  m2 <- as.lotri(df)
  expect_equal(attr(m2, "lotriOffDiagPriors"), attr(m, "lotriOffDiagPriors"))

  e <- as.expression(m)
  .lines <- as.list(e[[2]])[-1]
  expect_true(any(vapply(.lines, function(x) {
    is.call(x) && identical(x[[1]], quote(`~`)) &&
      is.call(x[[2]]) && identical(x[[2]][[1]], quote(`prior`)) &&
      identical(as.character(as.list(x[[2]])[-1]), c("eta.cl", "eta.v")) &&
      identical(x[[3]], str2lang("dnorm(0, 0.1)"))
  }, logical(1))))
})

test_that("print() shows a covariance prior distribution", {
  m <- lotri({
    eta.cl + eta.v ~ c(0.3, 0.05, 0.2)
    prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
  })
  expect_output(print(m), "covariance prior distributions")
})
