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

test_that(".lotriNamesAreCovPair() validates its own inputs directly", {
  ## the single call site in .lotriResolvePriors() already guarantees
  ## length(nms)==2 and both names present in dimnames(mat) before calling
  ## this, so these branches are otherwise unreachable through lotri()
  ## itself -- exercised directly here, the way a general-purpose helper
  ## (matching its sibling .lotriNamesAreBlock()'s own style) should be
  m <- lotri({ eta.cl + eta.v ~ c(0.3, 0.05, 0.2) })
  expect_false(.lotriNamesAreCovPair(m, c("eta.cl")))
  expect_false(.lotriNamesAreCovPair(m, c("eta.cl", "eta.cl")))
  expect_false(.lotriNamesAreCovPair(m, c("eta.cl", "nope")))
  expect_true(.lotriNamesAreCovPair(m, c("eta.cl", "eta.v")))
})

test_that("a duplicate prior on the same covariance pair is refused", {
  ## an EARLIER, generic duplicate check (keyed on the raw, un-stripped
  ## prior() names) already catches an IDENTICALLY-spelled repeat before
  ## the off-diagonal-storage-specific duplicate check further down is
  ## ever reached -- mixing the bare and `om.`-prefixed spellings makes the
  ## two lines look different to that earlier check (different raw names)
  ## while still resolving to the SAME stored key, so this exercises the
  ## off-diagonal-specific check on its own
  expect_error(
    lotri({
      eta.cl + eta.v ~ c(0.3, 0.05, 0.2)
      prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
      prior(om.eta.cl, om.eta.v) ~ dnorm(0, 0.2)
    }),
    "more than one prior given")
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

test_that("a whole-block prior and a marginal prior on one of its cells cannot coexist", {
  ## the family-based mutual-exclusivity check (wishart vs normal) cannot
  ## catch this: multiNormal() is itself family "normal" (same as a bare
  ## dnorm()), and dcauchy() is family "other" -- overlap has to be
  ## detected by direct block membership
  expect_error(
    lotri({
      eta.ka + eta.cl + eta.v ~ c(0.6, 0.01, 0.3, 0.02, 0.03, 0.2)
      prior(eta.ka, eta.cl, eta.v) ~ multiNormal(c(0, 0, 0), diag(3))
      prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
    }),
    "already has a whole-block prior")

  expect_error(
    lotri({
      eta.ka + eta.cl + eta.v ~ c(0.6, 0.01, 0.3, 0.02, 0.03, 0.2)
      prior(eta.ka, eta.cl, eta.v) ~ invWishart(4)
      prior(eta.cl, eta.v) ~ dcauchy(0, 0.1)
    }),
    "already has a whole-block prior")
})

test_that("a covariance-pair prior on one of two independent blocks survives lotriMatInv()'s block splitting", {
  ## exercises the sub-block extraction path in lotriMatInv() (only reached
  ## when a matrix actually contains more than one independent block) --
  ## the other tests above use a single connected block, so as.data.frame()
  ## never touches this code path for them
  m <- lotri({
    eta.a + eta.b ~ c(1, 0.5, 1)
    eta.c + eta.d ~ c(1, 0.5, 1)
    prior(eta.a, eta.b) ~ dnorm(0, 0.1)
  })
  df <- as.data.frame(m)
  .wAB <- which(df$name == "(eta.a,eta.b)")
  expect_length(.wAB, 1L)
  expect_equal(df$prior[.wAB], "dnorm(0, 0.1)")
  ## the unrelated second block's own covariance row carries no prior
  .wCD <- which(df$name == "(eta.c,eta.d)")
  expect_length(.wCD, 1L)
  expect_true(is.na(df$prior[.wCD]))

  ## round trip back through lotriMatInv() directly
  .lst <- lotriMatInv(m)
  expect_equal(length(.lst), 2L)
  .offDiags <- lapply(.lst, function(x) attr(x, "lotriOffDiagPriors"))
  .hasOffDiag <- vapply(.offDiags, function(x) !is.null(x) && length(x) > 0L, logical(1))
  expect_equal(sum(.hasOffDiag), 1L)
  expect_equal(.offDiags[[which(.hasOffDiag)]],
               c("(eta.a,eta.b)" = "dnorm(0, 0.1)"))
})
