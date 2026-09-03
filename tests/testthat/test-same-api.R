# Coverage for the exported `same()` contract downstream packages call
# (`lotriSameMap()`, `lotriSameBreak()`) and for the guard branches that
# a well formed model never reaches.

.sameIni <- function() {
  ## two 1x1 occasion parameters, occasion 2 repeating occasion 1
  data.frame(
    ntheta = c(1L, NA, NA, NA, NA),
    neta1 = c(NA, 1L, 2L, 3L, 4L),
    neta2 = c(NA, 1L, 2L, 3L, 4L),
    name = c("tka", "eta.ka", "a1", "b1", "a2"),
    lower = -Inf, est = c(0.5, 0.6, 0.1, 0.2, 0.1), upper = Inf,
    fix = FALSE, label = NA_character_, backTransform = NA_character_,
    condition = c(NA, "id", "id", "id", "id:same:a1"),
    prior = NA_character_, stringsAsFactors = FALSE)
}

test_that("lotriSameMap() validates its input", {
  expect_error(lotriSameMap("nope"), "must be a data.frame")
  expect_error(lotriSameMap(data.frame(a = 1)),
               "needs the 'name', 'neta1', 'neta2' and 'condition' columns")
  ## thetas only -- no etas to map
  .d <- .sameIni()[1, ]
  expect_equal(lotriSameMap(.d), integer(0))
})

test_that("lotriSameMap() reports the master by eta index", {
  ## 0 for an ordinary or master eta, else the index of the master it
  ## mirrors -- this is exactly what `rxSymInvCholCreate(same=)` takes
  ## a2 is eta 4 and mirrors a1, which is eta 2
  expect_equal(lotriSameMap(.sameIni()), c(0L, 0L, 0L, 2L))
  ## a frame with no repetition at all maps to all zeros
  .d <- .sameIni()
  .d$condition[.d$name == "a2"] <- "id"
  expect_equal(lotriSameMap(.d), c(0L, 0L, 0L, 0L))
})

test_that("lotriSameMap() refuses a pointer it cannot resolve", {
  ## a hand-edited frame can name a master that is not there ...
  .d <- .sameIni()
  .d$condition[.d$name == "a2"] <- "id:same:nosuch"
  expect_error(lotriSameMap(.d), "not a parameter")
  ## ... or one that is there twice.  Duplicate eta names are legal in
  ## lotri, so this is reachable, not merely defensive.
  .d <- .sameIni()
  .d$name[.d$name == "b1"] <- "a1"
  expect_error(lotriSameMap(.d), "ambiguous")
})

test_that("lotriSameBreak() leaves a frame alone when there is nothing to break", {
  expect_error(lotriSameBreak("nope", "a1"), "must be a data.frame")
  .d <- .sameIni()
  ## no etas named -> unchanged
  expect_identical(lotriSameBreak(.d, character(0)), .d)
  ## no `:same:` marker anywhere -> unchanged
  .d0 <- .d
  .d0$condition[.d0$name == "a2"] <- "id"
  expect_identical(lotriSameBreak(.d0, "a1"), .d0)
  ## a repetition that the edit does not touch -> unchanged
  expect_identical(lotriSameBreak(.d, "eta.ka"), .d)
})

test_that("lotriSameBreak() unlinks the family an edit touches", {
  .d <- .sameIni()
  .r <- lotriSameBreak(.d, "a1")
  expect_equal(.r$condition[.r$name == "a2"], "id")
  ## editing the COPY breaks it too -- the family is the unit
  .r <- lotriSameBreak(.d, "a2")
  expect_equal(.r$condition[.r$name == "a2"], "id")
})

test_that("lotriSameBreak() skips a marker whose master is not in a family", {
  ## An off-diagonal row can point at masters that no diagonal `:same:`
  ## row ever registered (a hand-edited frame).  It must be left alone
  ## rather than crash or be unlinked on a guess.
  .d <- data.frame(
    ntheta = NA_integer_,
    neta1 = c(1L, 2L, 3L, 4L, 4L),
    neta2 = c(1L, 2L, 3L, 4L, 3L),
    name = c("a", "b", "c1", "d1", "(c1,d1)"),
    lower = -Inf, est = c(1, 2, 1, 2, 0.1), upper = Inf,
    fix = FALSE, label = NA_character_, backTransform = NA_character_,
    condition = c("id", "id", "id", "id", "id:same:a:b"),
    prior = NA_character_, stringsAsFactors = FALSE)
  .r <- lotriSameBreak(.d, "c1")
  expect_equal(.r$condition[.r$name == "(c1,d1)"], "id:same:a:b")
})

test_that("as.lotri() refuses a diagonal pointer naming two elements", {
  .d <- .sameIni()
  ## `:same:a:b` is the spelling of an OFF diagonal pointer; on a
  ## diagonal row it is nonsense
  .d$condition[.d$name == "a2"] <- "id:same:a1:b1"
  expect_error(as.lotri(.d), "names 2 elements")
})

test_that("same() rejects a left hand side that is not parameter names", {
  ## lotri collects parse errors and re-raises them boxed, so the
  ## specific text is on stderr; check both
  expect_error(lotri({a + b ~ c(1, 0.1, 2); 1 ~ same()}), "syntax error")
  expect_message(try(lotri({a + b ~ c(1, 0.1, 2); 1 ~ same()}), silent = TRUE),
                 "left hand side")
  expect_error(lotri({a + b ~ c(1, 0.1, 2); f(x) ~ same()}), "syntax error")
})

test_that("a copy cannot carry an off-diagonal prior", {
  ## the diagonal case is covered elsewhere; this is the covariance one
  expect_error(lotri({
    a + b ~ c(1, 0.1, 2)
    c1 + d1 ~ same()
    prior(c1, d1) ~ dnorm(0, 0.1)
  }), "repeats an earlier block")
})

test_that("an off-diagonal prior survives being sliced with its block", {
  .m <- lotri({
    eta.cl + eta.v ~ c(0.3, 0.05, 0.2)
    prior(eta.cl, eta.v) ~ dnorm(0, 0.1)
  })
  ## keeping the pair keeps the prior ...
  .s <- .lotriSliceBlock(.m, 1:2)
  expect_equal(attr(.s, "lotriOffDiagPriors"),
               c("(eta.cl,eta.v)" = "dnorm(0, 0.1)"))
  expect_true(inherits(.s, "lotriFix"))
  ## ... dropping half the pair drops it, since the key no longer names
  ## two rows of this matrix
  .s <- .lotriSliceBlock(.m, 1L)
  expect_null(attr(.s, "lotriOffDiagPriors"))
})

test_that("a malformed lotriSame attribute is ignored, not obeyed", {
  .m <- lotri({a + b ~ c(1, 0.1, 2); c1 + d1 ~ same()})
  ## wrong length for the matrix -> no families at all
  expect_null(.lotriSameFamilies(.m, c(0L, 0L, 2L)))
  expect_null(.lotriSameFamilies(.m, NULL))
  ## an offset pointing before row 1 cannot describe a repetition
  expect_null(.lotriSameFamilies(.m, c(0L, 0L, 9L, 9L)))
})

test_that(".lotriSameOkFamily() refuses the families it cannot write back", {
  .m <- lotri({a + b ~ c(1, 0.1, 2); c1 + d1 ~ same()})
  .out <- c(0L, 0L, 2L, 2L)
  ## the real family is accepted
  expect_true(.lotriSameOkFamily(.m, .out, 3L, 2L, 2L, 4L))
  ## a master before the first row
  expect_false(.lotriSameOkFamily(.m, .out, 1L, 2L, 2L, 4L))
  ## the rows of the candidate do not all carry this offset
  expect_false(.lotriSameOkFamily(.m, c(0L, 0L, 2L, 3L), 3L, 2L, 2L, 4L))
  ## the "master" is itself a copy
  expect_false(.lotriSameOkFamily(.m, c(0L, 2L, 2L, 2L), 3L, 2L, 2L, 4L))
})

## ---------------------------------------------------------------------
## `.lotriSameEmit()` decides which blocks may be written back as
## `~ same()`.  A matrix rebuilt from a hand-edited data frame can carry
## an offset that is not re-parseable, so each guard is checked here
## directly -- going through `lotri()` cannot produce most of them.
## ---------------------------------------------------------------------

.blk <- function(n, v = 1, same = NULL, fix = NULL, labels = NULL,
                 tag = "a") {
  .m <- diag(n) * v
  .d <- paste0(tag, seq_len(n))
  dimnames(.m) <- list(.d, .d)
  if (!is.null(same)) attr(.m, "lotriSame") <- as.integer(same)
  if (!is.null(fix)) attr(.m, "lotriFix") <- fix
  if (!is.null(labels)) attr(.m, "lotriLabels") <- labels
  .m
}

test_that(".lotriSameEmit() accepts a well formed repetition", {
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(2, same = c(2, 2), tag = "b"))),
               c(FALSE, TRUE))
})

test_that(".lotriSameEmit() refuses an offset that is not a repetition", {
  ## no offsets at all
  expect_equal(.lotriSameEmit(list(.blk(2), .blk(2))), c(FALSE, FALSE))
  ## a zero mixed in: only part of the block claims to repeat
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(2, same = c(2, 0), tag = "b"))),
               c(FALSE, FALSE))
  ## rows of one block pointing different distances back
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(2, same = c(2, 3), tag = "b"))),
               c(FALSE, FALSE))
  ## an offset that lands in the middle of a block, not on its start
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(2, same = c(1, 1), tag = "b"))),
               c(FALSE, FALSE))
})

test_that(".lotriSameEmit() refuses a block of the wrong size", {
  ## lands on the start of block 1, but block 1 is 2x2 and this is 1x1
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(1, same = 2, tag = "b"))),
               c(FALSE, FALSE))
})

test_that(".lotriSameEmit() refuses a copy of a copy", {
  ## re-parsing `same()` always repeats the ORIGINAL block, so a block
  ## mirroring a mirror would come back with different offsets
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(2, same = c(2, 2), tag = "b"),
                                   .blk(2, same = c(2, 2), tag = "c"))),
               c(FALSE, TRUE, FALSE))
})

test_that(".lotriSameEmit() refuses a copy separated by a plain block", {
  ## `same()` repeats the IMMEDIATELY PRECEDING block, so anything
  ## between this and its master must itself be written as `same()`
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(2, v = 5, tag = "x"),
                                   .blk(2, same = c(4, 4), tag = "c"))),
               c(FALSE, FALSE, FALSE))
})

test_that(".lotriSameEmit() refuses a copy whose values drifted", {
  ## exact, not all.equal()'s default tolerance: a genuine copy is bit
  ## identical to its master
  expect_equal(.lotriSameEmit(list(.blk(2, v = 1, tag = "a"),
                                   .blk(2, v = 2, same = c(2, 2), tag = "b"))),
               c(FALSE, FALSE))
  expect_equal(.lotriSameEmit(list(.blk(2, v = 1, tag = "a"),
                                   .blk(2, v = 1 + 1e-9, same = c(2, 2),
                                        tag = "b"))),
               c(FALSE, FALSE))
})

test_that(".lotriSameEmit() refuses a copy whose fixed flags differ", {
  .f <- matrix(c(TRUE, FALSE, FALSE, TRUE), 2, 2)
  .g <- matrix(FALSE, 2, 2)
  ## one carries a fix matrix and the other does not
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(2, same = c(2, 2), fix = .f,
                                        tag = "b"))),
               c(FALSE, FALSE))
  expect_equal(.lotriSameEmit(list(.blk(2, fix = .f, tag = "a"),
                                   .blk(2, same = c(2, 2), tag = "b"))),
               c(FALSE, FALSE))
  ## both carry one, but they disagree
  expect_equal(.lotriSameEmit(list(.blk(2, fix = .f, tag = "a"),
                                   .blk(2, same = c(2, 2), fix = .g,
                                        tag = "b"))),
               c(FALSE, FALSE))
  ## ... and agreeing is fine
  expect_equal(.lotriSameEmit(list(.blk(2, fix = .f, tag = "a"),
                                   .blk(2, same = c(2, 2), fix = .f,
                                        tag = "b"))),
               c(FALSE, TRUE))
})

test_that(".lotriSameEmit() refuses a copy labelled anywhere but the end", {
  ## a `same()` line can carry only ONE trailing `label()`, which
  ## attaches to the last name; a label anywhere else would be dropped
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(2, same = c(2, 2),
                                        labels = c("lab", NA), tag = "b"))),
               c(FALSE, FALSE))
  ## a label on the LAST name survives the round trip
  expect_equal(.lotriSameEmit(list(.blk(2, tag = "a"),
                                   .blk(2, same = c(2, 2),
                                        labels = c(NA, "lab"), tag = "b"))),
               c(FALSE, TRUE))
})

test_that("as.lotri() handles an ini frame with no condition at all", {
  ## a minimal hand-built frame need not name a level; the `same()`
  ## splitter has to treat an NA condition as "not a repetition" rather
  ## than try to read a master out of it
  .d <- data.frame(
    ntheta = NA_integer_, neta1 = c(1L, 2L), neta2 = c(1L, 2L),
    name = c("a", "b"), lower = -Inf, est = c(1, 2), upper = Inf,
    fix = FALSE, label = NA_character_, backTransform = NA_character_,
    condition = NA_character_, prior = NA_character_,
    stringsAsFactors = FALSE)
  .m <- as.lotri(.d)
  expect_equal(as.numeric(.m), c(1, 0, 0, 2))
  expect_equal(dimnames(.m)[[1]], c("a", "b"))
  expect_null(attr(.m, "lotriSame", exact = TRUE))
})
