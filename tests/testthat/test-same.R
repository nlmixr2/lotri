test_that("same() repeats the preceding block", {

  .m <- lotri::lotri({
    iov.cl1 + iov.v1 ~ c(0.1,
                         0.01, 0.2)
    iov.cl2 + iov.v2 ~ same()
    iov.cl3 + iov.v3 ~ same()
  })

  expect_equal(dim(.m), c(6L, 6L))
  expect_equal(dimnames(.m)[[1]],
               c("iov.cl1", "iov.v1", "iov.cl2", "iov.v2",
                 "iov.cl3", "iov.v3"))
  ## the offsets are relative: each copy points back to the master
  expect_equal(attr(.m, "lotriSame"), c(0L, 0L, 2L, 2L, 4L, 4L))

  .b <- unclass(.m)[1:2, 1:2]
  expect_equal(unclass(.m)[3:4, 3:4], .b, ignore_attr = TRUE)
  expect_equal(unclass(.m)[5:6, 5:6], .b, ignore_attr = TRUE)
  ## and the copies do not covary with the master
  expect_true(all(unclass(.m)[1:2, 3:6] == 0))
})

test_that("same() works for a single parameter and chains", {

  .m <- lotri::lotri({
    a ~ 0.5
    b ~ same()
    d ~ same()
  })

  expect_equal(diag(unclass(.m)), c(a = 0.5, b = 0.5, d = 0.5))
  ## each `same()` repeats the ORIGINAL block, the way NONMEM chains SAME
  expect_equal(attr(.m, "lotriSame"), c(0L, 1L, 2L))
})

test_that("same() takes the master from either declaration form", {

  .plus <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })

  .line <- lotri::lotri({
    a ~ 1
    b ~ c(0.1, 2)
    c1 + d1 ~ same()
  })

  expect_equal(unclass(.plus), unclass(.line))
  expect_equal(attr(.plus, "lotriSame"), c(0L, 0L, 2L, 2L))
  expect_equal(attr(.line, "lotriSame"), c(0L, 0L, 2L, 2L))
})

test_that("same() repeats a declared block that contains a structural zero", {

  ## a 2x2 with a zero covariance is still a declared 2x2; inferring the
  ## block from connectivity would see two 1x1 blocks and reject this
  .m <- lotri::lotri({
    a + b ~ c(1,
              0, 1)
    c1 + d1 ~ same()
  })

  expect_equal(dim(.m), c(4L, 4L))
  expect_equal(attr(.m, "lotriSame"), c(0L, 0L, 2L, 2L))
})

test_that("a repeated block inherits the master's fixed flags", {

  .m <- lotri::lotri({
    a + b ~ fix(1,
                0.1, 2)
    c1 + d1 ~ same()
  })

  ## the repeated block is fixed like its master; the off block entries
  ## are structural zeros, not fixed elements
  expect_true(all(attr(.m, "lotriFix")[1:2, 1:2]))
  expect_true(all(attr(.m, "lotriFix")[3:4, 3:4]))
  expect_false(any(attr(.m, "lotriFix")[1:2, 3:4]))
})

test_that("same() works under a condition", {

  .m <- lotri::lotri({
    eta.ka ~ 0.6
    iov.cl1 + iov.v1 ~ c(0.1,
                         0.01, 0.2) | occ
    iov.cl2 + iov.v2 ~ same() | occ
  })

  expect_equal(names(.m), c("id", "occ"))
  expect_equal(dim(.m$occ), c(4L, 4L))
  expect_equal(attr(.m$occ, "lotriSame"), c(0L, 0L, 2L, 2L))
})

test_that("the condition column names the mirrored element", {

  .m <- lotri::lotri({
    iov.cl1 + iov.v1 ~ c(0.1,
                         0.01, 0.2)
    iov.cl2 + iov.v2 ~ same()
  })

  .df <- as.data.frame(.m)

  expect_equal(.df$condition,
               c("id", "id", "id",
                 "id:same:iov.cl1",
                 "id:same:iov.cl1:iov.v1",
                 "id:same:iov.v1"))
  ## a consumer that ignores the suffix still sees the right numbers
  expect_equal(.df$est, c(0.1, 0.01, 0.2, 0.1, 0.01, 0.2))
  ## no new column
  expect_equal(names(.df),
               c("ntheta", "neta1", "neta2", "name", "lower", "est",
                 "upper", "fix", "label", "backTransform", "condition",
                 "prior"))
})

test_that("same() round trips through the data frame and the expression", {

  .m <- lotri::lotri({
    tka <- 0.45
    eta.ka ~ 0.6
    iov.cl1 + iov.v1 ~ c(0.1,
                         0.01, 0.2)
    iov.cl2 + iov.v2 ~ same()
    iov.cl3 + iov.v3 ~ same()
  })

  .df <- as.data.frame(.m)

  ## data frame -> lotri -> data frame
  expect_equal(as.data.frame(lotri::as.lotri(.df)), .df)
  ## the block is rebuilt whole, not split into one matrix per suffix
  expect_equal(dim(lotri::as.lotri(.df)), c(7L, 7L))
  expect_equal(attr(lotri::as.lotri(.df), "lotriSame"),
               c(0L, 0L, 0L, 2L, 2L, 4L, 4L))

  ## lotri -> expression -> lotri
  expect_equal(as.data.frame(eval(as.expression(.m))), .df)

  .e <- as.character(as.expression(.m))
  expect_true(any(grepl("iov.cl2 + iov.v2 ~ same()", .e, fixed = TRUE)))
  .p <- as.character(lotri::lotriAsExpression(.m, plusNames = TRUE))
  expect_true(any(grepl("iov.cl2 + iov.v2 ~ same()", .p, fixed = TRUE)))
})

test_that("the master wins when a copy carries a different estimate", {

  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })

  .df <- as.data.frame(.m)
  .df$est[.df$name == "c1"] <- 999

  ## an estimator writes back only the block it estimated, so a repeated
  ## block takes its values from its master rather than from the frame
  expect_equal(unclass(lotri::as.lotri(.df))["c1", "c1"], 1)
})

test_that("lotriMat() carries the linkage across concatenation", {

  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })

  .r <- lotri::lotriMat(lotri::lotriMatInv(.m))
  expect_equal(attr(.r, "lotriSame"), c(0L, 0L, 2L, 2L))
  expect_equal(unclass(.r), unclass(.m), ignore_attr = TRUE)

  ## and through a list, which is the other route into the same C code
  expect_equal(attr(lotri::lotri(list(.m)), "lotriSame"), c(0L, 0L, 2L, 2L))

  ## a block that repeats, next to one that does not
  .r2 <- lotri::lotriMat(list(.m, lotri::lotri(z ~ 9)))
  expect_equal(attr(.r2, "lotriSame"), c(0L, 0L, 2L, 2L, 0L))
})

test_that("same() composes with the cnd(same=n) repeat property", {

  ## the two spellings are the same NONMEM concept at different
  ## granularities and must be usable together
  .n <- lotri::lotri(lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  }) | occ(same = 3L))

  .mm <- lotri::lotriMat(.n, format = "ETA[%d]", start = 1L)
  expect_equal(dim(.mm), c(12L, 12L))
  expect_equal(attr(.mm, "lotriSame"),
               rep(c(0L, 0L, 2L, 2L), 3))
})

test_that("lotriMatInv() reports a repeated block as a copy", {

  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })

  .l <- lotri::lotriMatInv(.m)
  expect_length(.l, 2L)
  expect_null(attr(.l[[1]], "lotriSame"))
  expect_equal(attr(.l[[2]], "lotriSame"), c(2L, 2L))
})

test_that("the exported helpers read the condition column", {

  .m <- lotri::lotri({
    eta.ka ~ 0.6
    iov.cl1 + iov.v1 ~ c(0.1,
                         0.01, 0.2)
    iov.cl2 + iov.v2 ~ same()
  })
  .df <- as.data.frame(.m)

  expect_equal(unique(lotri::lotriBaseCondition(.df$condition)), "id")
  expect_equal(lotri::lotriIsSame(.df$condition),
               c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  ## master eta index per eta, 0 when not a copy
  expect_equal(lotri::lotriSameMap(.df), c(0L, 0L, 0L, 2L, 3L))

  expect_equal(lotri::lotriBaseCondition(character(0)), character(0))
  expect_equal(lotri::lotriIsSame(character(0)), logical(0))
  expect_equal(lotri::lotriBaseCondition(NA_character_), NA_character_)
  expect_false(lotri::lotriIsSame(NA_character_))
})

test_that("lotriSameBreak() unlinks only the block that was touched", {

  .m <- lotri::lotri({
    a1 + b1 ~ c(1,
                0.1, 2)
    a2 + b2 ~ same()
    p1 + q1 ~ c(3,
                0.2, 4)
    p2 + q2 ~ same()
  })
  .df <- as.data.frame(.m)

  ## an unrelated parameter changes nothing
  expect_equal(lotri::lotriSameBreak(.df, "nope")$condition, .df$condition)
  expect_equal(lotri::lotriSameBreak(.df, character(0))$condition,
               .df$condition)

  ## touching either member of a block, or its master, unlinks the whole
  ## block -- and only that block
  for (.e in c("a2", "b2", "a1", "b1")) {
    .b <- lotri::lotriSameBreak(.df, .e)
    expect_equal(.b$condition[.b$name %in% c("a2", "b2", "(a2,b2)")],
                 rep("id", 3))
    expect_true(all(lotri::lotriIsSame(
      .b$condition[.b$name %in% c("p2", "q2", "(p2,q2)")])))
  }

  ## the values survive; only the linkage is dropped
  .b <- lotri::lotriSameBreak(.df, "a2")
  expect_equal(unclass(lotri::as.lotri(.b)), unclass(.m), ignore_attr = TRUE)
})

## a `lotri({})` block collects its per line errors and re-raises a single
## "lotri syntax errors above"; the specific text is emitted with
## `message()`, so it has to be captured to be asserted on
.expectLotriErr <- function(expr, regexp) {
  .msg <- NULL
  .r <- withCallingHandlers(
    tryCatch(expr, error = function(e) e),
    message = function(m) {
      .msg <<- c(.msg, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  testthat::expect_s3_class(.r, "error")
  testthat::expect_match(paste(.msg, collapse = "\n"), regexp,
                         fixed = TRUE, all = FALSE)
}

test_that("same() error paths", {

  .expectLotriErr(lotri::lotri({
    a + b ~ same()
  }), "'same()' has no block to repeat")

  .expectLotriErr(lotri::lotri({
    a + b ~ c(1, 0.1, 1)
    c1 ~ same()
  }), "it needs 2 names on the left, not 1")

  .expectLotriErr(lotri::lotri({
    a + b ~ c(1, 0.1, 1)
    c1 + d1 ~ same(2)
  }), "'same()' takes no arguments")

  .expectLotriErr(lotri::lotri({
    a + b ~ c(1, 0.1, 1)
    c1 + d1 ~ fix(same())
  }), "'same()' cannot be combined with 'fix()'")

  .expectLotriErr(lotri::lotri({
    a + b ~ c(1, 0.1, 1)
    c1 + d1 ~ same
  }), "did you mean 'same()'?")

  ## a repeat at a level that has not declared a block yet
  .expectLotriErr(lotri::lotri({
    a + b ~ c(1, 0.1, 1)
    c1 + d1 ~ same() | occ
  }), "'same()' has no block to repeat at level 'occ'")

  ## the master is at another level of variability
  .expectLotriErr(lotri::lotri({
    a + b ~ c(1, 0.1, 1) | occ
    c1 + d1 ~ same()
  }), "'same()' has no block to repeat")

  ## each extra argument to `lotri()` is parsed by its own call, so
  ## there is no shared parse state for `same()` to look back into
  expect_error(lotri::lotri(a + b ~ c(1, 0.1, 1), c1 + d1 ~ same()),
               "in the same '{}' block", fixed = TRUE)

  ## these are raised while the matrix is assembled, outside the per line
  ## collection, so they propagate with their own message
  expect_error(lotri::lotri({
    a + b ~ c(1, 0.1, 1)
    c1 + d1 ~ same()
  }, rcm = TRUE), "'rcm' cannot be used with 'same()'", fixed = TRUE)

  expect_error(lotri::lotri({
    a + b ~ c(1, 0.1, 1)
    c1 + d1 ~ same()
  }, cov = function(m) m),
  "a 'cov' function cannot be used with 'same()'", fixed = TRUE)
})

test_that("a variable named `same` still resolves", {

  ## `same` only becomes a keyword when it is not bound; otherwise the
  ## old behaviour (resolve it from the calling frame) has to hold
  same <- 0.5
  expect_equal(unclass(lotri::lotri({
    x ~ same
  })), matrix(0.5, 1, 1, dimnames = list("x", "x")))
})

test_that("a data frame with a broken same pointer is rejected", {

  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })
  .df <- as.data.frame(.m)

  .bad <- .df
  .bad$condition[.bad$name == "c1"] <- "id:same:nope"
  expect_error(lotri::as.lotri(.bad), "not in")

  ## a pointer that runs forwards is not a repeat
  .fwd <- .df
  .fwd$condition[.fwd$name == "a"] <- "id:same:d1"
  expect_error(lotri::as.lotri(.fwd), "must refer to an earlier parameter")
})

test_that("as.expression() only writes same() when it really re-parses", {

  ## `same()` carries no values of its own, so emitting it for a block
  ## that does not actually equal its claimed master would silently
  ## re-parse to a DIFFERENT matrix.  Such a matrix falls back to being
  ## written out with its explicit values.
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })

  .bad <- unclass(.m)
  .bad[3, 3] <- 99
  attr(.bad, "lotriSame") <- c(0L, 0L, 2L, 2L)
  class(.bad) <- c("lotriFix", "matrix", "array")

  expect_false(any(grepl("same()", as.character(as.expression(.bad)),
                         fixed = TRUE)))
  expect_equal(unclass(eval(as.expression(.bad))), unclass(.bad),
               ignore_attr = TRUE)

  ## a block that mirrors a mirror is likewise not re-emitted as
  ## `same()`, because a re-parsed `same()` always repeats the ORIGINAL
  ## block and would come back with different offsets
  .chain <- unclass(lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
    e1 + f1 ~ same()
  }))
  attr(.chain, "lotriSame") <- c(0L, 0L, 2L, 2L, 2L, 2L)
  class(.chain) <- c("lotriFix", "matrix", "array")
  expect_equal(unclass(eval(as.expression(.chain))), unclass(.chain),
               ignore_attr = TRUE)

  ## the ordinary case is unaffected
  expect_true(any(grepl("c1 + d1 ~ same()",
                        as.character(as.expression(.m)), fixed = TRUE)))
})

test_that("a repeat whose fixed flags differ is not re-emitted as same()", {

  .m <- lotri::lotri({
    a + b ~ fix(1,
                0.1, 2)
    c1 + d1 ~ same()
  })

  .bad <- unclass(.m)
  .f <- attr(.m, "lotriFix")
  .f[3:4, 3:4] <- FALSE
  attr(.bad, "lotriSame") <- c(0L, 0L, 2L, 2L)
  attr(.bad, "lotriFix") <- .f
  class(.bad) <- c("lotriFix", "matrix", "array")

  expect_false(any(grepl("same()", as.character(as.expression(.bad)),
                         fixed = TRUE)))
})

test_that("same() finds its block across an intervening line at another level", {

  ## `z ~ 5` lands at the id level; the `occ` block is still what the
  ## repeat at the occ level looks back to
  .m <- lotri::lotri({
    a1 + b1 ~ c(1,
                0.1, 2) | occ
    z ~ 5
    a2 + b2 ~ same() | occ
  })

  expect_equal(names(.m), c("id", "occ"))
  expect_equal(dim(.m$id), c(1L, 1L))
  expect_equal(dim(.m$occ), c(4L, 4L))
  expect_equal(attr(.m$occ, "lotriSame"), c(0L, 0L, 2L, 2L))
})

test_that("a conditioned block round trips through as.expression()", {

  ## the line form accumulates through `lastN`, which the conditioned
  ## scalar branch never set: `a ~ 1 | occ` followed by
  ## `b ~ c(0.1, 2) | occ` silently produced a 1x1 `occ` instead of a 2x2
  .p <- lotri::lotri({
    tk <- 1
    eta ~ 0.6
    a + b ~ c(1,
              0.1, 2) | occ
  })

  .r <- eval(as.expression(.p))
  expect_equal(dim(.r$occ), c(2L, 2L))
  expect_equal(as.data.frame(.r), as.data.frame(.p))

  ## and with a repeated block on top of it
  .m <- lotri::lotri({
    tk <- 1
    eta ~ 0.6
    a + b ~ c(1,
              0.1, 2) | occ
    c1 + d1 ~ same() | occ
  })

  expect_equal(as.data.frame(eval(as.expression(.m))), as.data.frame(.m))
})

test_that("a repeated block under a condition names its master correctly", {

  ## `.env$eta1` counts globally across conditions while `.matNames` is
  ## the condition's own dimnames; mixing the two named the wrong
  ## parameter (here `occ:same:b` and even `occ:same:c1`)
  .m <- lotri::lotri({
    tk <- 1
    eta ~ 0.6
    a + b ~ c(1,
              0.1, 2) | occ
    c1 + d1 ~ same() | occ
  })

  .df <- as.data.frame(.m)
  expect_equal(.df$condition,
               c(NA, "id", "occ", "occ", "occ",
                 "occ:same:a", "occ:same:a:b", "occ:same:b"))
  expect_equal(as.data.frame(lotri::as.lotri(.df)), .df)
  expect_equal(attr(lotri::as.lotri(.df)$occ, "lotriSame"),
               c(0L, 0L, 2L, 2L))
})

test_that("a label on a repeated block survives as.expression()", {

  ## `same()` carries no values, so the trailing `label()` is the only
  ## place the copy's label can go; without it the label was silently
  ## dropped on the round trip
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    label("master")
    c1 + d1 ~ same()
    label("copy")
  })

  expect_equal(attr(.m, "lotriLabels"), c(NA, "master", NA, "copy"))

  .e <- as.character(as.expression(.m))
  expect_true(any(grepl("c1 + d1 ~ same()", .e, fixed = TRUE)))
  expect_true(any(grepl('label("copy")', .e, fixed = TRUE)))

  .r <- eval(as.expression(.m))
  expect_equal(attr(.r, "lotriLabels"), c(NA, "master", NA, "copy"))
  expect_equal(as.data.frame(.r), as.data.frame(.m))
})

test_that("a label same() cannot carry blocks the same() shorthand", {

  ## only ONE trailing `label()` is expressible, and it attaches to the
  ## last name; a label on an earlier row of the copy would be lost, so
  ## the block is written out with its explicit values instead
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })
  .bad <- unclass(.m)
  attr(.bad, "lotriSame") <- c(0L, 0L, 2L, 2L)
  attr(.bad, "lotriLabels") <- c(NA, NA, "first", NA)
  class(.bad) <- c("lotriFix", "matrix", "array")

  expect_false(any(grepl("same()", as.character(as.expression(.bad)),
                         fixed = TRUE)))
  expect_equal(attr(eval(as.expression(.bad)), "lotriLabels"),
               c(NA, NA, "first", NA))
})

test_that("a prior cannot be put on a repeated block", {

  ## a copy is not a parameter of its own -- it IS the block it mirrors,
  ## so a prior on it would duplicate or silently contradict the
  ## master's prior
  expect_error(lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    prior(c1) ~ dnorm(0, 1)
  }), "put the prior on 'a'", fixed = TRUE)

  expect_error(lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    prior(c1, d1) ~ lkjCorr(2)
  }), "same()", fixed = TRUE)

  ## the case that was silently accepted: two DIFFERENT priors on what
  ## is one estimated parameter
  expect_error(lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    prior(a) ~ dnorm(0, 1)
    prior(c1) ~ dnorm(5, 9)
  }), "same()", fixed = TRUE)

  ## on the master it is fine, and still round trips
  .m <- lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    prior(a) ~ dnorm(0, 1)
  })
  expect_equal(attr(.m, "lotriPriors"), c("dnorm(0, 1)", NA, NA, NA))
  expect_equal(as.data.frame(eval(as.expression(.m))), as.data.frame(.m))

  expect_error(lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    prior(a, b) ~ lkjCorr(2)
  }), NA)
})

test_that("a repeated block survives lotriSep() nesting", {

  ## the end-to-end IOV shape: a correlated 2x2 repeated with `same()`,
  ## then that whole level stamped once per occasion by `lotriSep()`
  .n <- lotri::lotri({
    eta.ka ~ 0.6
    a + b ~ c(1,
              0.1, 2) | occ
    c1 + d1 ~ same() | occ
  })

  .s <- lotri::lotriSep(.n, above = c(id = 1L), below = c(occ = 2L))
  expect_equal(attr(.s$below$occ, "lotriSame"), c(0L, 0L, 2L, 2L))

  .mm <- lotri::lotriMat(.s$below, format = "ETA[%d]", start = 1L)
  ## 1 id eta + 2 occasions x 4 etas
  expect_equal(dim(.mm), c(9L, 9L))
  expect_equal(attr(.mm, "lotriSame"),
               c(0L, rep(c(0L, 0L, 2L, 2L), 2)))
})

test_that("the condition base follows the `default` argument", {

  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })

  expect_equal(as.data.frame(.m, default = "occ")$condition,
               c("occ", "occ", "occ",
                 "occ:same:a", "occ:same:a:b", "occ:same:b"))
})

test_that("a repeated block at the default level survives a condition", {

  ## `.lotriExprCnd()` moves the default level's parse state into its own
  ## environment; it carried the rows but not the offsets, so ANY
  ## conditioned line in the same block silently dropped the linkage.
  ## The values stayed right, so the only visible effect was a model
  ## with twice as many free omega parameters as it should have.
  for (.f in list(
    function() lotri::lotri({
      z ~ 3 | occ
      a + b ~ c(1, 0.1, 2)
      c1 + d1 ~ same()
    }),
    function() lotri::lotri({
      a + b ~ c(1, 0.1, 2)
      c1 + d1 ~ same()
      z ~ 3 | occ
    }),
    function() lotri::lotri({
      a + b ~ c(1, 0.1, 2)
      c1 + d1 ~ same()
      y + z ~ c(3, 0.2, 4) | occ
    }))) {
    .m <- .f()
    expect_equal(attr(.m$id, "lotriSame"), c(0L, 0L, 2L, 2L))
  }

  ## the guards read the same attribute, so they were bypassed too
  expect_error(lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    z ~ 3 | occ
  }, rcm = TRUE), "'rcm' cannot be used with 'same()'", fixed = TRUE)

  expect_error(lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    z ~ 3 | occ
  }, cov = function(x) x * 2),
  "a 'cov' function cannot be used with 'same()'", fixed = TRUE)
})

test_that("same() is not emitted across an intervening independent block", {

  ## a re-parsed `same()` repeats the IMMEDIATELY PRECEDING block, so a
  ## copy separated from its master by an unrelated block must be
  ## written out with its values.  Reachable through the documented data
  ## frame contract, since `as.lotri()` accepts any earlier parameter as
  ## the master.
  .m <- lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    p1 + q1 ~ c(3, 0.2, 4)
    c1 + d1 ~ c(1, 0.1, 2)
    label("x")
  })
  .df <- as.data.frame(.m)
  .df$condition[.df$name == "c1"] <- "id:same:a"
  .df$condition[.df$name == "(c1,d1)"] <- "id:same:a:b"
  .df$condition[.df$name == "d1"] <- "id:same:b"
  .l <- lotri::as.lotri(.df)
  ## `same()` repeats the immediately preceding block, so a pointer that
  ## skips `p1`/`q1` is not expressible and the linkage is dropped when
  ## the frame is read -- the values are untouched
  expect_null(attr(.l, "lotriSame"))

  expect_false(any(grepl("same()", as.character(as.expression(.l)),
                         fixed = TRUE)))
  .rt <- eval(as.expression(.l))
  expect_equal(unclass(.rt), unclass(.l), ignore_attr = TRUE)
  ## the copy keeps ITS values, not the intervening block's
  expect_equal(unname(.rt["c1", "c1"]), 1)
  expect_equal(unname(.rt["d1", "d1"]), 2)

  ## a genuine chain, where every block in between repeats the same
  ## master, is still emitted
  .chain <- lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    e1 + f1 ~ same()
  })
  expect_true(any(grepl("e1 + f1 ~ same()",
                        as.character(as.expression(.chain)), fixed = TRUE)))
})

test_that("a block sliced away from its master degrades gracefully", {

  ## the offsets are relative, so an extracted copy points before row 1;
  ## the linkage is not representable standalone and must not be
  ## indexed out of range into a plausible looking wrong name
  .m <- lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
  })
  .b <- lotri::lotriMatInv(.m)[[2]]
  expect_equal(attr(.b, "lotriSame"), c(2L, 2L))

  expect_equal(as.data.frame(.b)$condition, rep("id", 3))
  expect_error(lotri::as.lotri(as.data.frame(.b)), NA)
  expect_output(print(.b), "c1", fixed = TRUE)
  expect_false(any(grepl("repeat",
                         capture.output(print(.b)), fixed = TRUE)))
})

test_that("blocks that merely agree to a tolerance are not collapsed", {

  .m <- lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
  })
  .near <- unclass(.m)
  .near[3, 3] <- 1 + 1e-10
  attr(.near, "lotriSame") <- c(0L, 0L, 2L, 2L)
  class(.near) <- c("lotriFix", "matrix", "array")

  ## `all.equal()`'s default tolerance would have called these equal and
  ## written `same()`, changing the value on the round trip
  expect_false(any(grepl("same()", as.character(as.expression(.near)),
                         fixed = TRUE)))
  expect_equal(unname(eval(as.expression(.near))[3, 3]), 1 + 1e-10)
})

test_that("the whole-omega prior shorthand skips repeated blocks", {

  ## `~ invWishart(4)` applies to every FREE block.  A copy is not a
  ## free block -- it is the block it repeats, which the shorthand has
  ## already reached -- so without skipping it the shorthand tripped the
  ## prior-on-a-copy rejection and became unusable with `same()`.
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
    ~ invWishart(4)
  })

  expect_equal(attr(.m, "lotriPriors"),
               c("invWishart(4)", NA, NA, NA))
  expect_equal(as.data.frame(lotri::as.lotri(as.data.frame(.m))),
               as.data.frame(.m))
  expect_equal(as.data.frame(eval(as.expression(.m))), as.data.frame(.m))

  ## a chain gets exactly one prior, on the block that is estimated
  .chain <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
    e1 + f1 ~ same()
    ~ invWishart(4)
  })
  expect_equal(sum(!is.na(attr(.chain, "lotriPriors"))), 1L)

  ## a fixed master leaves the shorthand with nothing to apply to, which
  ## is the pre-existing error rather than a `same()` specific one
  expect_error(lotri::lotri({
    a + b ~ fix(1, 0.1, 2)
    c1 + d1 ~ same()
    ~ invWishart(4)
  }), "no omega to apply it to")
})

test_that("no prior form can target a repeated block", {

  ## the marginal form is caught where the priors are attached, but a
  ## joint theta+omega prior (NONMEM TNPRI) is stored on the THETA row
  ## and never reaches that code, so it needs its own check
  expect_error(lotri::lotri({
    tk <- 1
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    tk + om.c1 ~ c(1, 0.01, 0.02)
  }), "put the prior on 'a'", fixed = TRUE)

  expect_error(lotri::lotri({
    tk <- 1
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    tk + om.c1 + om.d1 ~ c(1, 0.01, 0.02, 0.01, 0.02, 0.03)
  }), "same()", fixed = TRUE)

  ## the `om.` shorthand and the explicit `prior()` form too
  expect_error(lotri::lotri({
    tk <- 1
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    om.c1 ~ 0.01
  }), "same()", fixed = TRUE)

  expect_error(lotri::lotri({
    tk <- 1
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    prior(om.c1) ~ dnorm(0, 1)
  }), "same()", fixed = TRUE)

  ## all of them are fine on the block that is actually estimated
  expect_error(lotri::lotri({
    tk <- 1
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    tk + om.a ~ c(1, 0.01, 0.02)
  }), NA)

  expect_error(lotri::lotri({
    tk <- 1
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    om.a ~ 0.01
  }), NA)
})

test_that("same() repeats the master after any transformation", {

  ## the block is copied AFTER `sd()`/`var()`/`cor()`/`chol()` have been
  ## turned into a covariance, so the copy carries the same covariance
  for (.e in list(quote(lotri::lotri({
                    a + b ~ sd(1, 0.1, 2)
                    c1 + d1 ~ same()
                  })),
                  quote(lotri::lotri({
                    a + b ~ var(1, 0.1, 2)
                    c1 + d1 ~ same()
                  })),
                  quote(lotri::lotri({
                    a + b ~ cor(1, 0.1, 2)
                    c1 + d1 ~ same()
                  })),
                  quote(lotri::lotri({
                    a + b ~ chol(1, 0.1, 2)
                    c1 + d1 ~ same()
                  })))) {
    .m <- eval(.e)
    expect_equal(attr(.m, "lotriSame"), c(0L, 0L, 2L, 2L))
    expect_equal(unclass(.m)[3:4, 3:4], unclass(.m)[1:2, 1:2],
                 ignore_attr = TRUE)
  }
})

test_that("same() cannot repeat a matrix() literal", {

  ## a `matrix()` literal is held aside and merged after the block is
  ## parsed, so it is not a block `same()` can look back at
  .expectLotriErr(lotri::lotri({
    matrix(c(1, 0.1, 0.1, 2), 2, 2,
           dimnames = list(c("a", "b"), c("a", "b")))
    c1 + d1 ~ same()
  }), "'same()' has no block to repeat")
})

test_that("same() is not emitted past a block that is itself written out", {

  ## a chain only re-parses correctly if every block in it is actually
  ## WRITTEN as `same()`.  A block that carries the right offsets but
  ## fails its own guard -- here a label on a name `same()` cannot carry
  ## -- is written with explicit values, and a later `same()` would then
  ## repeat THAT block instead of the master.
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
    e1 + f1 ~ same()
  })

  .x <- unclass(.m)
  .x[3, 3] <- 99
  .x[4, 4] <- 88
  attr(.x, "lotriSame") <- c(0L, 0L, 2L, 2L, 4L, 4L)
  class(.x) <- c("lotriFix", "matrix", "array")

  ## block 2 no longer equals its master, so it is written out; block 3
  ## must then be written out too rather than repeating block 2
  expect_equal(diag(eval(as.expression(.x))),
               c(a = 1, b = 2, c1 = 99, d1 = 88, e1 = 1, f1 = 2))

  ## the same through the documented data frame route, via a label
  .df <- as.data.frame(.m)
  .df$label[.df$name == "c1"] <- "mid label"
  .l <- lotri::as.lotri(.df)
  .rt <- eval(as.expression(.l))
  expect_equal(unclass(.rt), unclass(.l), ignore_attr = TRUE)
  expect_equal(attr(.rt, "lotriLabels")[3], "mid label")
  ## and the two routes agree that the linkage is gone, rather than the
  ## data frame keeping one the expression cannot write
  expect_false(any(lotri::lotriIsSame(as.data.frame(.l)$condition)))
  expect_equal(as.data.frame(.rt), as.data.frame(.l))
})

test_that("labels and fixed flags are judged by the shared view", {

  ## these used to be checked only by the emitter, so a copy carrying a
  ## label it could not express kept its `:same:` in the data frame
  ## while the expression wrote explicit values -- two parameter counts
  ## for one matrix
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })

  .lab <- unclass(.m)
  attr(.lab, "lotriSame") <- c(0L, 0L, 2L, 2L)
  attr(.lab, "lotriLabels") <- c(NA, NA, "first", NA)
  class(.lab) <- c("lotriFix", "matrix", "array")
  expect_false(any(lotri::lotriIsSame(as.data.frame(.lab)$condition)))
  expect_null(attr(eval(as.expression(.lab)), "lotriSame"))

  .fx <- unclass(.m)
  .f <- matrix(FALSE, 4, 4, dimnames = dimnames(.fx))
  .f[3, 3] <- TRUE
  attr(.fx, "lotriSame") <- c(0L, 0L, 2L, 2L)
  attr(.fx, "lotriFix") <- .f
  class(.fx) <- c("lotriFix", "matrix", "array")
  expect_false(any(lotri::lotriIsSame(as.data.frame(.fx)$condition)))
  expect_null(attr(eval(as.expression(.fx)), "lotriSame"))

  ## print reads the same view, so it cannot claim a repetition the
  ## other two dropped
  expect_false(any(grepl(" repeat ", capture.output(print(.lab)),
                         fixed = TRUE)))
})

test_that("an offset that no longer describes a mirror is dropped", {

  ## the offsets are relative, so a list of blocks that has been
  ## reordered or had blocks dropped can leave one pointing at an
  ## unrelated block that merely happens to be in range.  Writing that
  ## out as a `:same:` pointer would make the bogus linkage real and let
  ## the "master" overwrite the values on the way back in.
  .m <- lotri::lotri({
    z ~ 9
    a ~ 1
    b ~ same()
  })
  expect_equal(attr(.m, "lotriSame"), c(0L, 0L, 1L))

  .dropped <- lotri::lotriMat(lotri::lotriMatInv(.m)[c(1, 3)])
  expect_equal(diag(unclass(.dropped)), c(z = 9, b = 1))
  ## `b` must stay 1, not silently become `z`
  expect_equal(diag(unclass(lotri::as.lotri(as.data.frame(.dropped)))),
               c(z = 9, b = 1))
  expect_false(any(lotri::lotriIsSame(as.data.frame(.dropped)$condition)))
})

test_that("a block with mixed offsets does not index out of range", {

  ## `as.lotri()` derives offsets per diagonal, so a hand written frame
  ## can leave one row of a block mirrored and the others not.  The
  ## unmirrored end of a covariance cell used to index at or below zero,
  ## which silently duplicated rows through `data.frame()` recycling.
  .m <- lotri::lotri({
    a + b + c ~ c(1,
                  0.1, 2,
                  0.1, 0.1, 3)
    d1 + e1 + f1 ~ c(4,
                     0.2, 5,
                     0.2, 0.2, 6)
    label("L")
  })
  .df <- as.data.frame(.m)
  .df$condition[.df$name == "f1"] <- "id:same:b"
  .b <- lotri::lotriMatInv(lotri::lotriEst(lotri::as.lotri(.df),
                                           drop = TRUE))[[2]]

  .d <- as.data.frame(.b)
  expect_equal(nrow(.d), 6L)
  expect_equal(anyDuplicated(.d$name), 0L)
  expect_false(any(lotri::lotriIsSame(.d$condition)))
})

test_that("two repeated blocks with the same offset stay separate", {

  ## both copies sit two rows after their own master, so grouping the
  ## offsets by VALUE rather than by contiguous run would fuse the two
  ## families and drop both linkages
  .f <- lotri::lotri({
    a1 + b1 ~ c(1,
                0.1, 2)
    a2 + b2 ~ same()
    p1 + q1 ~ c(3,
                0.2, 4)
    p2 + q2 ~ same()
  })

  expect_equal(attr(.f, "lotriSame"),
               c(0L, 0L, 2L, 2L, 0L, 0L, 2L, 2L))
  expect_equal(as.data.frame(.f)$condition,
               c("id", "id", "id",
                 "id:same:a1", "id:same:a1:b1", "id:same:b1",
                 "id", "id", "id",
                 "id:same:p1", "id:same:p1:q1", "id:same:q1"))
  expect_equal(as.data.frame(eval(as.expression(.f))), as.data.frame(.f))
})

test_that("a valid repetition survives an invalid one next to it", {

  ## dropping a master makes two families adjacent.  Grouping the
  ## offsets by contiguous ROW RUN fused them, and the all-or-nothing
  ## check then killed the genuine family along with the bogus one.
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
    e1 + f1 ~ c(5,
                0.5, 6)
    g1 + h1 ~ same()
  })
  .d <- lotri::lotriMat(lotri::lotriMatInv(.m)[c(1, 2, 4)])
  expect_equal(attr(.d, "lotriSame"), c(0L, 0L, 2L, 2L, 2L, 2L))

  .df <- as.data.frame(.d)
  ## c1/d1 still mirrors a/b exactly and must keep its pointer ...
  expect_equal(.df$condition[.df$name == "c1"], "id:same:a")
  expect_equal(.df$condition[.df$name == "d1"], "id:same:b")
  ## ... while g1/h1, whose master went away, must not
  expect_false(any(lotri::lotriIsSame(.df$condition[.df$name %in%
                                                      c("g1", "h1")])))
  expect_equal(as.data.frame(lotri::as.lotri(.df)), .df)

  ## when the values DO coincide the offsets are read at face value: the
  ## chain resolves and all three blocks are one estimated 2x2
  .m2 <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
    e1 + f1 ~ c(1,
                0.1, 2)
    g1 + h1 ~ same()
  })
  .d2 <- lotri::lotriMat(lotri::lotriMatInv(.m2)[c(1, 2, 4)])
  .df2 <- as.data.frame(.d2)
  expect_equal(.df2$condition[.df2$name == "g1"], "id:same:a")
  expect_equal(as.data.frame(lotri::as.lotri(.df2)), .df2)
  expect_equal(as.data.frame(eval(as.expression(lotri::as.lotri(.df2)))),
               .df2)
})

test_that("a repeated block with a structural zero round trips", {

  ## `lotriMatInv()` splits on connectivity, so a declared block with a
  ## covariance of exactly zero comes back as two blocks and the
  ## `same()` line was written against the wrong master -- or, once that
  ## was guarded, not written at all.  The declared boundaries are
  ## recovered from the offsets instead.
  .m <- lotri::lotri({
    a + b ~ c(1,
              0, 2)
    c1 + d1 ~ same()
  })
  expect_equal(attr(.m, "lotriSame"), c(0L, 0L, 2L, 2L))

  .e <- as.character(as.expression(.m))
  expect_true(any(grepl("c1 + d1 ~ same()", .e, fixed = TRUE)))
  expect_equal(attr(eval(as.expression(.m)), "lotriSame"),
               c(0L, 0L, 2L, 2L))
  expect_equal(as.data.frame(eval(as.expression(.m))), as.data.frame(.m))

  ## a partial zero inside a 3x3 too
  .m3 <- lotri::lotri({
    a + b + cc ~ c(1,
                   0.1, 2,
                   0, 0, 3)
    d1 + e1 + f1 ~ same()
  })
  expect_equal(attr(eval(as.expression(.m3)), "lotriSame"),
               c(0L, 0L, 0L, 3L, 3L, 3L))
  expect_equal(as.data.frame(eval(as.expression(.m3))), as.data.frame(.m3))
})

test_that("print() agrees with the values it printed", {

  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
    e1 + f1 ~ c(5,
                0.5, 6)
    g1 + h1 ~ same()
  })
  .d <- lotri::lotriMat(lotri::lotriMatInv(.m)[c(1, 2, 4)])

  .out <- capture.output(print(.d))
  ## g1/h1 is 5, 0.5, 6 and c1/d1 is 1, 0.1, 2 -- it must not claim the
  ## first repeats the second
  expect_true(any(grepl("c1, d1 repeat a, b", .out, fixed = TRUE)))
  expect_false(any(grepl("g1", .out[grepl("repeat", .out)], fixed = TRUE)))
})

test_that("a repeated block must be separated from the rest of the matrix", {

  ## the declared boundaries are recovered from the offsets, so a
  ## hand written frame can claim a master range that cuts through a
  ## larger dense block.  Forcing a block boundary there dropped the
  ## covariance that crossed it from the emitted expression.
  .m <- lotri::lotri({
    a + b + cc ~ c(1,
                   0.1, 2,
                   0.2, 0.3, 3)
    d1 + e1 ~ c(1,
                0.1, 2)
    label("L")
  })
  .df <- as.data.frame(.m)
  .df$condition[.df$name == "d1"] <- "id:same:a"
  .df$condition[.df$name == "(d1,e1)"] <- "id:same:a:b"
  .df$condition[.df$name == "e1"] <- "id:same:b"
  .l <- lotri::lotriEst(lotri::as.lotri(.df), drop = TRUE)

  ## the 3x3 stays whole ...
  expect_equal(vapply(.lotriSameSplit(.l),
                      function(x) dim(x)[1], integer(1)),
               c(3L, 2L))
  ## ... so the covariance that crosses the claimed boundary survives
  .rt <- eval(as.expression(.l))
  expect_equal(unname(unclass(.rt)[1, 3]), 0.2)
  expect_equal(unclass(.rt), unclass(.l), ignore_attr = TRUE)
})

test_that("a forced block boundary may not cut a covariance that spans it", {

  ## the family's own rows can be decoupled while the boundary it forces
  ## still separates two rows that DO covary.  `x1` and `x3` are each
  ## separated, but cutting after `x3` splits `x2` from `x4`.
  .m <- lotri::lotri({
    x1 ~ 2
    x2 + x3 + x4 ~ c(1.6,
                     0, 2,
                     0.25, 0, 1.9)
    label("L")
  })
  .df <- as.data.frame(.m)
  .df$condition[.df$name == "x3"] <- "id:same:x1"
  .l <- lotri::lotriEst(lotri::as.lotri(.df), drop = TRUE)

  ## the 3x3 must stay whole ...
  expect_equal(vapply(.lotriSameSplit(.l),
                      function(x) dim(x)[1], integer(1)),
               c(1L, 3L))
  ## ... so the covariance that spans the cut survives
  expect_equal(unname(unclass(.l)["x2", "x4"]), 0.25)
  expect_equal(unname(unclass(eval(as.expression(.l)))["x2", "x4"]), 0.25)
})

test_that("a valid family survives a bogus one in the same run", {

  ## `x3` mirrors `x1` legitimately; `x4` is hand pointed at `x2`, a
  ## copy, so it is bogus.  Both carry offset 2, and reading the run as
  ## one 2 wide family threw the valid one away with the bogus one.
  .m <- lotri::lotri({
    x1 ~ 2.3
    x2 ~ same()
    x3 ~ same()
    x4 ~ 2.3
    label("L")
  })
  .df <- as.data.frame(.m)
  .df$condition[.df$name == "x4"] <- "id:same:x2"
  .l <- lotri::lotriEst(lotri::as.lotri(.df), drop = TRUE)

  ## `x4 -> x2 -> x1` resolves to `x4 -> x1`, and every value is 2.3, so
  ## all three are one estimated parameter.  What matters is that the
  ## legitimate `x3 -> x1` is NOT collateral damage, and that the three
  ## consumers agree.
  expect_equal(attr(.l, "lotriSame"), c(0L, 1L, 2L, 3L))

  .df2 <- as.data.frame(.l)
  expect_equal(.df2$condition,
               c("id", "id:same:x1", "id:same:x1", "id:same:x1"))
  expect_equal(attr(eval(as.expression(.l)), "lotriSame"),
               c(0L, 1L, 2L, 3L))
  expect_equal(as.data.frame(eval(as.expression(.l))), .df2)
  expect_true(any(grepl("x3 repeat x1", capture.output(print(.l)),
                        fixed = TRUE)))
})

test_that("as.expression() never sees an offset the family view rejected", {

  ## `.lotriSameSplit()` stamps the validated offsets onto the blocks it
  ## returns, so the emitter cannot re-invent a linkage that
  ## `as.data.frame()` dropped
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })
  .b <- lotri::lotriMatInv(.m)[[2]]        # offsets point before row 1
  expect_equal(attr(.b, "lotriSame"), c(2L, 2L))
  expect_false(any(lotri::lotriIsSame(as.data.frame(.b)$condition)))
  .rt <- eval(as.expression(.b))
  expect_null(attr(.rt, "lotriSame"))
  ## with the linkage gone the result is a plain matrix, so compare the
  ## numbers rather than the data frame (which would dispatch to the
  ## base `as.data.frame()` method)
  expect_equal(unclass(.rt), unclass(.b), ignore_attr = TRUE)
})

test_that("the data frame cannot encode a linkage same() cannot write", {

  ## `same()` repeats the IMMEDIATELY PRECEDING block, so a pointer that
  ## skips over an unrelated block is not expressible.  Encoding it
  ## anyway made the two round trips report different numbers of
  ## estimated parameters: the data frame kept the linkage, the
  ## expression could not write it.
  .m <- lotri::lotri({
    p1 ~ 1
    p2 ~ 2
    p3 ~ 1
    label("L")
  })
  .df <- as.data.frame(.m)
  .df$condition[.df$name == "p3"] <- "id:same:p1"   # skips p2
  .l <- lotri::lotriEst(lotri::as.lotri(.df), drop = TRUE)

  expect_false(any(lotri::lotriIsSame(as.data.frame(.l)$condition)))
  expect_null(attr(eval(as.expression(.l)), "lotriSame"))
  expect_equal(unclass(eval(as.expression(.l))), unclass(.l),
               ignore_attr = TRUE)

  ## a chain, where the block in between repeats the SAME master, is
  ## still expressible and must survive
  .chain <- lotri::lotri({
    a ~ 1
    b ~ same()
    d ~ same()
  })
  expect_equal(attr(.chain, "lotriSame"), c(0L, 1L, 2L))
  expect_equal(as.data.frame(eval(as.expression(.chain))),
               as.data.frame(.chain))
})

test_that("a NONMEM style chain naming the preceding copy is normalised", {

  ## NONMEM chains `SAME`, so a consumer writing this encoding naturally
  ## names the block just before -- which is itself a copy.  `same()`
  ## re-parses as repeating the ORIGINAL, so the two spellings mean the
  ## same thing and the chain resolves rather than being dropped.
  .df <- as.data.frame(lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
    e1 + f1 ~ same()
  }))
  .df$condition[.df$name == "e1"] <- "id:same:c1"
  .df$condition[.df$name == "(e1,f1)"] <- "id:same:c1:d1"
  .df$condition[.df$name == "f1"] <- "id:same:d1"

  .l <- lotri::as.lotri(.df)
  expect_equal(attr(.l, "lotriSame"), c(0L, 0L, 2L, 2L, 4L, 4L))
  ## all three blocks are one estimated 2x2, not two or three
  expect_equal(as.data.frame(.l)$condition,
               c("id", "id", "id",
                 "id:same:a", "id:same:a:b", "id:same:b",
                 "id:same:a", "id:same:a:b", "id:same:b"))
  expect_equal(as.data.frame(lotri::as.lotri(as.data.frame(.l))),
               as.data.frame(.l))
  expect_equal(as.data.frame(eval(as.expression(.l))), as.data.frame(.l))
})

test_that("every consumer agrees on the linkage, over random matrices", {

  ## the property that ties the three consumers together: whatever
  ## `lotriSameMap()` says about a matrix, it must still say after a
  ## round trip through the expression -- otherwise one object has two
  ## parameter counts depending on the route taken.  This is the check
  ## that catches the whole class of defects the reviews kept finding.
  skip_on_cran()
  set.seed(20260902)

  for (.it in seq_len(400)) {
    .n <- sample(2:6, 1)
    .m <- diag(round(runif(.n, 0.5, 3), 2))
    for (.k in seq_len(sample(0:3, 1))) {
      .i <- sample(.n, 1)
      .j <- sample(.n, 1)
      if (.i != .j) {
        .v <- round(runif(1, -0.3, 0.3), 2)
        .m[.i, .j] <- .v
        .m[.j, .i] <- .v
      }
    }
    ## make genuine repetitions likely, not just random noise
    if (runif(1) < 0.5 && .n >= 4) {
      .k <- sample(seq_len(floor(.n / 2)), 1)
      .m[(.k + 1):(2 * .k), (.k + 1):(2 * .k)] <- .m[1:.k, 1:.k]
    }
    dimnames(.m) <- list(paste0("p", seq_len(.n)), paste0("p", seq_len(.n)))

    .same <- integer(.n)
    for (.k in seq_len(sample(1:2, 1))) {
      .i <- sample(seq_len(.n), 1)
      .d <- sample(seq_len(.n - 1), 1)
      if (.i - .d >= 1) .same[.i] <- .d
    }
    if (all(.same == 0L)) next

    .x <- .m
    attr(.x, "lotriSame") <- .same
    if (runif(1) < 0.4) {
      .lb <- rep(NA_character_, .n)
      .lb[sample(seq_len(.n), 1)] <- "L"
      attr(.x, "lotriLabels") <- .lb
    }
    class(.x) <- c("lotriFix", "matrix", "array")

    .info <- paste("lotriSame =", paste(.same, collapse = ","))
    .df <- as.data.frame(.x)
    expect_equal(anyDuplicated(.df$name), 0L, info = .info)

    ## values survive both routes, exactly
    .viaDf <- lotri::as.lotri(.df)
    .viaEx <- eval(as.expression(.x))
    expect_equal(unclass(.viaEx), unclass(.x), ignore_attr = TRUE,
                 info = .info)
    expect_equal(unclass(.viaDf)[seq_len(.n), seq_len(.n)], unclass(.x),
                 ignore_attr = TRUE, info = .info)

    ## and the two routes agree on which parameters are free.  A matrix
    ## whose linkage was dropped comes back unclassed, so the offsets
    ## are compared directly rather than through `as.data.frame()`.
    .oDf <- attr(.viaDf, "lotriSame")
    .oEx <- attr(.viaEx, "lotriSame")
    expect_equal(if (is.null(.oEx)) integer(.n) else as.integer(.oEx),
                 if (is.null(.oDf)) integer(.n) else as.integer(.oDf),
                 info = .info)
  }
})

test_that("rcm() says so when it drops a repetition", {

  ## `lotri(..., rcm=TRUE)` refuses a repeated block outright; the
  ## exported `rcm()` still permutes, but losing the linkage changes how
  ## many parameters the matrix is understood to estimate, so it is not
  ## dropped quietly the way `lotriFix`/`lotriLabels` are
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })
  expect_warning(lotri::rcm(unclass(.m)), "drops the 'same()' repetition",
                 fixed = TRUE)
  expect_silent(lotri::rcm(unclass(lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
  }))))
})

test_that("a hand set double offset vector is carried by lotriMat()", {

  ## the R side coerces with `as.integer()`, so the C concatenation
  ## accepts a double rather than silently dropping the repetition
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })
  .d <- unclass(.m)
  attr(.d, "lotriSame") <- c(0, 0, 2, 2)
  expect_equal(attr(lotri::lotriMat(list(.d)), "lotriSame"),
               c(0L, 0L, 2L, 2L))
})

test_that("a plus-form block after a line-form block is placed correctly", {

  ## A line-form block leaves `eta1` pointing at its FIRST row, the rest
  ## counted in `lastN`.  A plus-form block after one used to write over
  ## those rows.  Alone that was a loud "dimnames not equal to array
  ## extent"; with `same()` after it, `.fCallSame()`'s own `.resetLastN`
  ## made the lengths line up again and it became a SILENTLY wrong
  ## matrix -- a fabricated variance, an overwritten estimate, and the
  ## copy mirroring the wrong pair.
  .mix <- lotri::lotri({
    a ~ 1
    b ~ c(0.1, 2)
    c1 + d1 ~ c(3,
                0.2, 4)
    e1 + f1 ~ same()
  })
  .plus <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ c(3,
                0.2, 4)
    e1 + f1 ~ same()
  })

  ## the two spellings are the same model
  expect_equal(unclass(.mix), unclass(.plus), ignore_attr = TRUE)
  expect_equal(attr(.mix, "lotriSame"), c(0L, 0L, 0L, 0L, 2L, 2L))
  expect_equal(unname(unclass(.mix)["b", "b"]), 2)
  expect_equal(unname(unclass(.mix)["d1", "d1"]), 4)

  ## and without `same()` it is no longer an error either
  expect_equal(unclass(lotri::lotri({
    a ~ 1
    b ~ c(0.1, 2)
    c1 + d1 ~ c(3,
                0.2, 4)
  })), unclass(.plus)[1:4, 1:4], ignore_attr = TRUE)

  ## the same under a condition
  .cnd <- lotri::lotri({
    a ~ 1 | occ
    b ~ c(0.1, 2) | occ
    c1 + d1 ~ c(3,
                0.2, 4) | occ
    e1 + f1 ~ same() | occ
  })
  expect_equal(unclass(.cnd$occ), unclass(.plus), ignore_attr = TRUE)
  expect_equal(attr(.cnd$occ, "lotriSame"), c(0L, 0L, 0L, 0L, 2L, 2L))
})

test_that("a non-integral offset is not truncated into a repetition", {

  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })
  .d <- unclass(.m)
  attr(.d, "lotriSame") <- c(0, 0, 2.7, 2.7)
  ## 2.7 is not an offset; truncating it to 2 would invent a repetition
  expect_equal(attr(lotri::lotriMat(list(.d)), "lotriSame"),
               c(0L, 0L, 0L, 0L))
})

test_that("an arithmetic right hand side after a line-form block lands right", {

  ## `d ~ 0.1*2` is a call of length 3, so it falls past `.fCallTilde()`
  ## into `.fcallTildeLhsSum()`'s numeric branch -- which, like the
  ## plus-form branch, has to settle a preceding line-form block.  Alone
  ## it was a loud error; with `same()` after it the lengths lined up
  ## again and it became a silently wrong matrix.
  .m <- lotri::lotri({
    a ~ 1
    b ~ c(0.1, 2)
    d ~ 0.1 * 2
    e ~ same()
  })

  expect_equal(diag(unclass(.m)), c(a = 1, b = 2, d = 0.2, e = 0.2))
  expect_equal(unname(unclass(.m)["a", "b"]), 0.1)
  ## `e` repeats `d`, not `b`
  expect_equal(attr(.m, "lotriSame"), c(0L, 0L, 0L, 1L))

  ## and the plain form is no longer an error
  expect_equal(unclass(lotri::lotri({
    a ~ 1
    b ~ c(0.1, 2)
    d ~ 0.1 * 2
  })), unclass(.m)[1:3, 1:3], ignore_attr = TRUE)
})

test_that("a 1x1 block under a condition settles the line form too", {

  ## the `.num == 1` branch of `.lotri1()` set the counter before the
  ## reset could use it, so the fix was dead for 1x1 blocks -- reachable
  ## only through the conditioned route, since an unconditioned scalar
  ## goes through `.fCallTilde()`
  .m <- lotri::lotri({
    a ~ 1 | occ
    b ~ c(0.1, 2) | occ
    c1 ~ 3 | occ
    d1 ~ same() | occ
  })

  expect_equal(diag(unclass(.m$occ)), c(a = 1, b = 2, c1 = 3, d1 = 3))
  expect_equal(unname(unclass(.m$occ)["a", "b"]), 0.1)
  expect_equal(attr(.m$occ, "lotriSame"), c(0L, 0L, 0L, 1L))
})

test_that("lotriEst(drop=TRUE) keeps the class the attributes need", {

  ## the class was kept only for `lotriFix`, so a repeated block with no
  ## fixed elements came back unclassed with the attribute orphaned --
  ## every consumer then dispatched to the default method and the
  ## repetition was gone
  .m <- lotri::lotri({
    tk <- 1
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  })
  .d <- lotri::lotriEst(.m, drop = TRUE)

  expect_s3_class(.d, "lotriFix")
  expect_equal(attr(.d, "lotriSame"), c(0L, 0L, 2L, 2L))
  expect_true(any(lotri::lotriIsSame(as.data.frame(.d)$condition)))

  ## a labelled matrix keeps its class for the same reason
  .l <- lotri::lotriEst(lotri::lotri({
    tk <- 1
    a ~ 1
    label("LA")
  }), drop = TRUE)
  expect_s3_class(.l, "lotriFix")

  ## and a plain one still loses it
  expect_false(inherits(lotri::lotriEst(lotri::lotri({
    tk <- 1
    a ~ 1
  }), drop = TRUE), "lotriFix"))
})

test_that("a frame pairing a prior with a repeated block is refused", {

  ## `lotri()` refuses a prior on a copy at parse time; reading a frame
  ## that pairs the two would otherwise build an object that cannot be
  ## written back out -- `as.expression()` emitted both the `same()`
  ## line and the `prior()` line, and re-parsing rejected it
  .df <- as.data.frame(lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
  }))

  .diag <- .df
  .diag$prior[.diag$name == "c1"] <- "dnorm(1, 2)"
  expect_error(lotri::as.lotri(.diag), "cannot carry its own prior")

  .off <- .df
  .off$prior[.off$name == "(c1,d1)"] <- "dnorm(1, 2)"
  expect_error(lotri::as.lotri(.off), "cannot carry its own prior")

  ## on the master it is fine and still round trips
  .ok <- as.data.frame(lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 + d1 ~ same()
    prior(a) ~ dnorm(0, 1)
  }))
  expect_error(lotri::as.lotri(.ok), NA)
  expect_equal(as.data.frame(lotri::as.lotri(.ok)), .ok)
})

test_that("random programs parse to the matrix a reference model predicts", {

  ## The attribute-level property test above cannot reach the parser: it
  ## builds matrices by setting attributes.  Two reviews found parse
  ## time offset bugs that it structurally could not see, and a
  ## differential test (`same()` spelling vs the explicit one) cannot
  ## see them either, because both spellings corrupt identically.  Only
  ## an independent reference model catches them.
  skip_on_cran()
  set.seed(20260902)

  .mkblk <- function(k) {
    .m <- diag(round(runif(k, 0.5, 3), 2), nrow = k)
    if (k > 1) {
      for (.i in 2:k) {
        for (.j in 1:(.i - 1)) {
          .v <- round(runif(1, -0.3, 0.3), 2)
          .m[.i, .j] <- .v
          .m[.j, .i] <- .v
        }
      }
    }
    .m
  }
  .lower <- function(m) {
    .out <- numeric(0)
    for (.i in seq_len(nrow(m))) {
      for (.j in seq_len(.i)) .out <- c(.out, m[.i, .j])
    }
    .out
  }

  for (.it in seq_len(300)) {
    .lines <- character(0)
    .blockOf <- integer(0)
    .blocks <- list()
    .nmOf <- list()
    .sameOf <- integer(0)
    .nms <- character(0)
    .prev <- NULL
    .prevK <- 0L
    .ctr <- 0L

    for (.b in seq_len(sample(1:3, 1))) {
      .useSame <- !is.null(.prev) && runif(1) < 0.35
      .k <- if (.useSame) .prevK else sample(1:3, 1)
      .nm <- paste0("v", .ctr + seq_len(.k))
      .ctr <- .ctr + .k
      .nLinesBefore <- length(.lines)
      if (.useSame) {
        .m <- .prev
        .lines <- c(.lines, paste0(paste(.nm, collapse = " + "), " ~ same()"))
      } else {
        .m <- .mkblk(.k)
        .style <- sample(if (.k == 1) {
          c("scalar", "arith", "cvec")
        } else {
          c("plus", "line")
        }, 1)
        .lines <- c(.lines, switch(
          .style,
          scalar = paste0(.nm, " ~ ", .m[1, 1]),
          arith = paste0(.nm, " ~ ", .m[1, 1], "*1"),
          cvec = paste0(.nm, " ~ c(", .m[1, 1], ")"),
          plus = paste0(paste(.nm, collapse = " + "), " ~ c(",
                        paste(.lower(.m), collapse = ", "), ")"),
          line = vapply(seq_len(.k), function(.i) {
            paste0(.nm[.i], " ~ c(",
                   paste(.m[.i, seq_len(.i)], collapse = ", "), ")")
          }, character(1), USE.NAMES = FALSE)))
        .prev <- .m
        .prevK <- .k
      }
      .blocks[[length(.blocks) + 1L]] <- .m
      .sameOf <- c(.sameOf,
                   if (.useSame) length(.blocks) - 1L else 0L)
      .nmOf[[length(.blocks)]] <- .nm
      .blockOf <- c(.blockOf,
                    rep(length(.blocks), length(.lines) - .nLinesBefore))
      .nms <- c(.nms, .nm)
    }

    ## the condition is chosen per BLOCK, not per program: a bug that
    ## drags an unconditioned block into a later level can only show up
    ## when the two are mixed
    .lvl <- vapply(seq_along(.blocks), function(.i) {
      if (runif(1) < 0.35) "occ" else "id"
    }, character(1), USE.NAMES = FALSE)
    ## a `same()` copy repeats the block before it, so it has to sit at
    ## that block's level -- `same()` only ever looks within one level
    for (.i in seq_along(.sameOf)) {
      if (.sameOf[.i] > 0L) .lvl[.i] <- .lvl[.sameOf[.i]]
    }
    .lines <- unlist(lapply(seq_along(.lines), function(.i) {
      if (.lvl[.blockOf[.i]] == "occ") paste0(.lines[.i], " | occ") else .lines[.i]
    }), use.names = FALSE)
    .txt <- paste0("lotri::lotri({", paste(.lines, collapse = "; "), "})")

    .got <- eval(parse(text = .txt))

    ## every level must contain exactly the parameters declared at it
    for (.l in unique(.lvl)) {
      .want <- unlist(.nmOf[.lvl == .l], use.names = FALSE)
      .have <- if (is.list(.got)) {
        dimnames(unclass(.got[[.l]]))[[1]]
      } else {
        dimnames(unclass(.got))[[1]]
      }
      expect_equal(.have, .want, info = .txt)

      ## and hold the values the reference model predicts
      .n <- length(.want)
      .exp <- matrix(0, .n, .n, dimnames = list(.want, .want))
      .p <- 0L
      for (.bi in which(.lvl == .l)) {
        .m <- .blocks[[.bi]]
        .k <- nrow(.m)
        .exp[.p + seq_len(.k), .p + seq_len(.k)] <- .m
        .p <- .p + .k
      }
      .gm <- if (is.list(.got)) unclass(.got[[.l]]) else unclass(.got)
      expect_equal(.gm[seq_len(.n), seq_len(.n), drop = FALSE], .exp,
                   ignore_attr = TRUE, info = .txt)
    }
  }
})

test_that("a block declared without a condition stays at the default level", {

  ## A conditioned line was taken to CONTINUE the default level's open
  ## block on a value-count test alone, with no check that the left hand
  ## side is a single name.  A plus-form block whose value count matched
  ## hijacked the default level: its rows were moved into the condition,
  ## so parameters declared with no `| cnd` silently ended up at that
  ## level of variability.  Before the row-counter fixes this blew up
  ## loudly; afterwards the lengths lined up and it went quiet.
  .m <- lotri::lotri({
    z11 ~ 1.1
    z12 ~ c(0.11, 2.2)
    z21 + z22 ~ c(1.1,
                  0.11, 2.2) | occ
    z31 ~ 1.1
  })

  expect_equal(dimnames(unclass(.m$id))[[1]], c("z11", "z12", "z31"))
  expect_equal(dimnames(unclass(.m$occ))[[1]], c("z21", "z22"))

  ## with a repeated block on top
  .s <- lotri::lotri({
    a ~ 1
    b ~ c(0.1, 2)
    c1 + d1 ~ c(3,
                0.2, 4) | occ
    e1 + f1 ~ same() | occ
  })
  expect_equal(dimnames(unclass(.s$id))[[1]], c("a", "b"))
  expect_equal(dimnames(unclass(.s$occ))[[1]],
               c("c1", "d1", "e1", "f1"))
  expect_equal(attr(.s$occ, "lotriSame"), c(0L, 0L, 2L, 2L))

  ## the genuine line-form continuation under a condition is unaffected
  .c <- lotri::lotri({
    a ~ 1 | occ
    b ~ c(0.1, 2) | occ
  })
  expect_equal(dim(unclass(.c$occ)), c(2L, 2L))
  expect_equal(unname(unclass(.c$occ)[1, 2]), 0.1)

  ## and so is a scalar opened at a level, then continued there
  .k <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2)
    c1 ~ 3 | occ
    d1 ~ c(0.2, 4) | occ
  })
  expect_equal(dimnames(unclass(.k$id))[[1]], c("a", "b"))
  expect_equal(dimnames(unclass(.k$occ))[[1]], c("c1", "d1"))
})

test_that("same() finds its level across an intervening other level", {

  ## the level was looked up only if it was the most recently parsed
  ## condition, so an unrelated line at another level in between hid a
  ## block that is plainly still there
  .m <- lotri::lotri({
    a + b ~ c(1,
              0.1, 2) | occ
    z ~ 1 | id
    c1 + d1 ~ same() | occ
  })

  expect_equal(dimnames(unclass(.m$occ))[[1]], c("a", "b", "c1", "d1"))
  expect_equal(dimnames(unclass(.m$id))[[1]], "z")
  expect_equal(attr(.m$occ, "lotriSame"), c(0L, 0L, 2L, 2L))
  expect_equal(unclass(.m$occ)[3:4, 3:4], unclass(.m$occ)[1:2, 1:2],
               ignore_attr = TRUE)

  ## a level that genuinely has no block still says so
  .expectLotriErr(lotri::lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same() | occ
  }), "'same()' has no block to repeat at level 'occ'")
})
