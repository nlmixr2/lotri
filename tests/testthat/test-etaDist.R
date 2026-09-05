test_that("the eta distribution catalog is a subset of the prior catalog", {
  .e <- lotriEtaDists()
  .p <- lotriPriorDists()
  expect_true(all(.e$name %in% .p$name))
  expect_true(all(.e$kind == "univariate"))
  expect_true(all(nzchar(.e$quantile)))
  ## every declared parameter appears in its own quantile template
  for (.i in seq_len(nrow(.e))) {
    if (!nzchar(.e$parNames[.i])) next
    for (.p2 in strsplit(.e$parNames[.i], ",", fixed=TRUE)[[1]]) {
      expect_true(grepl(paste0("{", .p2, "}"), .e$quantile[.i], fixed=TRUE),
                  info=paste0(.e$name[.i], ": ", .p2))
    }
  }
  ## and every template uses the uniform
  expect_true(all(grepl("{u}", .e$quantile, fixed=TRUE)))
})

test_that("dist() declares a distribution on a single eta", {
  .x <- lotri({
    eta.cl ~ 1
    dist(eta.cl) ~ dgamma(shape=aCl, rate=bCl)
  })
  expect_equal(attr(.x, "lotriEtaDists"), "dgamma(aCl, bCl)")
  ## the alias parses the same
  .y <- lotri({
    eta.cl ~ 1
    etaDist(eta.cl) ~ dgamma(shape=aCl, rate=bCl)
  })
  expect_equal(attr(.y, "lotriEtaDists"), attr(.x, "lotriEtaDists"))
  ## arguments are canonicalized into positional order
  .z <- lotri({
    eta.cl ~ 1
    dist(eta.cl) ~ dgamma(rate=bCl, shape=aCl)
  })
  expect_equal(attr(.z, "lotriEtaDists"), "dgamma(aCl, bCl)")
})

test_that("a declared eta lives in a correlation block", {
  expect_error(lotri({
    eta.cl ~ 0.09
    dist(eta.cl) ~ dgamma(a, b)
  }), "diagonals estimated at one")
  expect_error(lotri({
    eta.cl + eta.v ~ c(0.09,
                       0.01, 1)
    dist(eta.cl) ~ dgamma(a, b)
  }), "diagonals estimated at one")
  ## the message shows the block form that would work
  expect_error(lotri({
    eta.cl + eta.v ~ c(0.09,
                       0.01, 1)
    dist(eta.cl) ~ dgamma(a, b)
  }), "eta.cl + eta.v ~ c(1, 0.1, 1)", fixed=TRUE)
  ## an undeclared block is untouched by the rule
  expect_silent(lotri({
    eta.cl ~ 0.09
    eta.v ~ 1
    dist(eta.v) ~ dgamma(a, b)
  }))
  expect_error(lotri({
    eta.cl + eta.v ~ c(1,
                       1, 1)
    dist(eta.cl) ~ dgamma(a, b)
  }), "between -1 and 1")
})

test_that("a declared distribution implies a unit variance", {
  ## the declaration already fixes the marginal, and the latent scale is
  ## standard normal by construction, so `eta.cl ~ 1` alongside it is a
  ## repetition of something that could not be anything else
  .x <- lotri({
    dist(eta.cl) ~ dgamma(shape=aCl, rate=bCl)
  })
  expect_equal(dimnames(.x)[[1]], "eta.cl")
  expect_equal(unname(as.matrix(.x)[1, 1]), 1)
  expect_equal(attr(.x, "lotriEtaDists"), "dgamma(aCl, bCl)")
  ## an unbraced single line too
  .y <- lotri(dist(eta.cl) ~ dgamma(shape=aCl, rate=bCl))
  expect_equal(as.matrix(.y), as.matrix(.x))
  ## it mixes with ordinary random effects, in either order
  for (.m in list(lotri({
                    eta.ka ~ 0.6
                    dist(eta.cl) ~ dgamma(a, b)
                  }),
                  lotri({
                    dist(eta.cl) ~ dgamma(a, b)
                    eta.ka ~ 0.6
                  }))) {
    expect_setequal(dimnames(.m)[[1]], c("eta.ka", "eta.cl"))
    expect_equal(unname(as.matrix(.m)["eta.cl", "eta.cl"]), 1)
    expect_equal(unname(as.matrix(.m)["eta.ka", "eta.ka"]), 0.6)
    ## implied and declared stay uncorrelated
    expect_equal(unname(as.matrix(.m)["eta.ka", "eta.cl"]), 0)
  }
  ## two of them are two separate 1x1 blocks, not one 2x2
  .z <- lotri({
    dist(eta.cl) ~ dgamma(a, b)
    dist(eta.v) ~ dexp(r)
  })
  expect_equal(unname(as.matrix(.z)["eta.cl", "eta.v"]), 0)
  ## and it round trips
  expect_equal(attr(eval(as.expression(.x)), "lotriEtaDists"),
               attr(.x, "lotriEtaDists"))
})

test_that("a correlated block still has to be written out", {
  ## the correlation has nowhere else to go, so that block IS written --
  ## and then its unit diagonal is checked rather than assumed
  .x <- lotri({
    eta.cl + eta.v1 ~ c(1,
                        0.5, 1)
    dist(eta.cl) ~ dgamma(aCl, bCl)
    dist(eta.v1) ~ dgamma(aV1, bV1)
  })
  expect_equal(unname(as.matrix(.x)["eta.cl", "eta.v1"]), 0.5)
  ## an explicit non-unit variance is still refused, and for a lone
  ## declared random effect the message says to drop the line
  expect_error(lotri({
    eta.cl ~ 0.09
    dist(eta.cl) ~ dgamma(a, b)
  }), "drop the 'eta.cl ~ ...' line", fixed=TRUE)
})

test_that("bad declarations are refused", {
  ## a `dist()` on a name that is not otherwise declared no longer errors
  ## -- it declares that random effect, with the unit variance the
  ## distribution implies
  .x <- lotri({
    eta.cl ~ 1
    dist(eta.v) ~ dgamma(a, b)
  })
  expect_setequal(dimnames(.x)[[1]], c("eta.cl", "eta.v"))
  expect_equal(unname(as.matrix(.x)["eta.v", "eta.v"]), 1)
  expect_error(lotri({
    eta.cl ~ 1
    dist(eta.cl) ~ dfoo(a, b)
  }))
  ## R's dt() is not Stan's student_t()
  expect_error(lotri({
    eta.cl ~ 1
    dist(eta.cl) ~ dt(6)
  }))
  ## a real family with no available quantile function
  expect_error(lotri({
    eta.cl ~ 1
    dist(eta.cl) ~ skewNormal(0, 1, 2)
  }))
  ## a real family that is not univariate
  expect_error(lotri({
    eta.cl ~ 1
    dist(eta.cl) ~ multiNormal(a, b)
  }))
  expect_error(lotri({
    eta.cl ~ 1
    dist(eta.cl) ~ dgamma(a, b)
    dist(eta.cl) ~ dexp(a)
  }), "more than one distribution")
  expect_error(lotri({
    eta.cl ~ fix(1)
    dist(eta.cl) ~ dgamma(a, b)
  }), "is fixed")
  expect_error(lotri({
    eta.cl ~ 1
    eta.v ~ 1
    dist(eta.cl, eta.v) ~ dgamma(a, b)
  }))
  expect_error(lotri({
    eta.cl ~ 1
    dist(eta.cl) ~ stats::dgamma(a, b)
  }))
  expect_error(lotri({
    eta.cl ~ 1
    dist(eta.cl) ~ dgamma(shp=1, rate=2)
  }))
})

test_that("a same() copy cannot declare its own distribution", {
  expect_error(lotri({
    eta.cl + eta.v ~ c(1,
                       0.5, 1)
    eta.cl2 + eta.v2 ~ same()
    dist(eta.cl2) ~ dgamma(a, b)
  }), "repeats")
})

test_that("the declaration round trips through data.frame and expression", {
  .x <- lotri({
    eta.cl + eta.v1 ~ c(1,
                        0.5, 1)
    dist(eta.cl) ~ dgamma(aCl, bCl)
    dist(eta.v1) ~ studentT(nu, 0, 1)
  })
  .d <- as.data.frame(.x)
  expect_true(any(names(.d) == "etaDist"))
  expect_equal(.d$etaDist[.d$name == "eta.cl"], "dgamma(aCl, bCl)")
  expect_equal(.d$etaDist[.d$name == "eta.v1"], "studentT(nu, 0, 1)")
  ## an off diagonal never carries one
  expect_true(is.na(.d$etaDist[.d$neta1 != .d$neta2]))
  .y <- as.lotri(.d)
  expect_equal(attr(.y, "lotriEtaDists"), attr(.x, "lotriEtaDists"))
  ## and the expression re-evaluates to the same object
  .z <- eval(as.expression(.x))
  expect_equal(attr(.z, "lotriEtaDists"), attr(.x, "lotriEtaDists"))
  expect_equal(as.matrix(.z), as.matrix(.x))
})

test_that("a model with no declaration keeps its old data.frame exactly", {
  .x <- lotri({
    eta.cl + eta.v1 ~ c(0.1,
                        0.01, 0.1)
  })
  expect_false(any(names(as.data.frame(.x)) == "etaDist"))
})

test_that("declarations survive rcm reordering and lotriMatInv", {
  .x <- lotri({
    eta.v ~ 1
    eta.cl ~ 1
    dist(eta.cl) ~ dgamma(aCl, bCl)
  }, rcm=TRUE)
  .dn <- dimnames(.x)[[1]]
  expect_equal(attr(.x, "lotriEtaDists")[.dn == "eta.cl"], "dgamma(aCl, bCl)")
  .lst <- lotriMatInv(.x)
  .found <- unlist(lapply(.lst, function(.m) {
    .d <- attr(.m, "lotriEtaDists")
    if (is.null(.d)) return(NULL)
    stats::setNames(.d, dimnames(.m)[[1]])
  }))
  expect_equal(unname(.found[names(.found) == "eta.cl"]), "dgamma(aCl, bCl)")
})

test_that("declarations work with conditions", {
  .x <- lotri({
    eta.cl ~ 1 | id
    dist(eta.cl) ~ dgamma(aCl, bCl)
    iov.cl ~ 0.1 | occ
  })
  expect_equal(attr(.x$id, "lotriEtaDists"), "dgamma(aCl, bCl)")
  expect_null(attr(.x$occ, "lotriEtaDists"))
  .d <- as.data.frame(.x)
  expect_equal(.d$etaDist[.d$name == "eta.cl"], "dgamma(aCl, bCl)")
  expect_true(is.na(.d$etaDist[.d$name == "iov.cl"]))
  ## and back again
  expect_equal(attr(as.lotri(.d)$id, "lotriEtaDists"), "dgamma(aCl, bCl)")
})
