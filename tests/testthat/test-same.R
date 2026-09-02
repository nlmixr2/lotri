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
