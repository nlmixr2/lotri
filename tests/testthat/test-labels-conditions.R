test_that("a label follows the level its parameter was declared at", {

  ## Which level a trailing `label()` belongs to used to be decided by
  ## whether the DEFAULT level had any labels yet.  Once it did, a
  ## conditioned line's label landed on the default level and overwrote
  ## the label already sitting there.
  .m <- lotri::lotri({
    tp <- 1
    a ~ 0.5
    label("LA")
    b ~ 0.6 | occ
    label("LB")
  })

  expect_equal(attr(.m$id, "lotriLabels"), "LA")
  expect_equal(attr(.m$occ, "lotriLabels"), "LB")

  ## and both reach the data frame, which only ever read the labels of a
  ## single matrix and so lost them all for a conditioned object
  .df <- as.data.frame(.m)
  expect_equal(.df$label[.df$name == "a"], "LA")
  expect_equal(.df$label[.df$name == "b"], "LB")

  ## still round trips
  expect_equal(as.data.frame(lotri::as.lotri(.df)), .df)

  ## the conditioned-only and unconditioned-only spellings are unchanged
  expect_equal(attr(lotri::lotri({
    a ~ 0.5 | occ
    label("L")
  })$occ, "lotriLabels"), "L")

  .plain <- lotri::lotri({
    tp <- 1
    a ~ 0.5
    label("LA")
    b ~ 0.6
    label("LB")
  })
  expect_equal(attr(.plain, "lotriLabels"), c("LA", "LB"))
})

test_that("labels survive on more than one conditioned level", {

  .m <- lotri::lotri({
    tp <- 1
    a ~ 0.5
    label("LA")
    b ~ 0.6 | occ
    label("LB")
    d ~ 0.7 | inv
    label("LD")
  })

  expect_equal(attr(.m$id, "lotriLabels"), "LA")
  expect_equal(attr(.m$occ, "lotriLabels"), "LB")
  expect_equal(attr(.m$inv, "lotriLabels"), "LD")

  .df <- as.data.frame(.m)
  expect_equal(.df$label[!is.na(.df$neta1)], c("LA", "LB", "LD"))
  expect_equal(as.data.frame(lotri::as.lotri(.df)), .df)
})

test_that("a single non-default level keeps its name", {

  ## an occasion-only model came back from the data frame as a bare
  ## matrix, i.e. looking like an id level one
  .m <- lotri::lotri({
    tpop <- 1
    e1 ~ 0.54 | occ2
  })
  .df <- as.data.frame(.m)

  .back <- lotri::as.lotri(.df)
  expect_equal(names(.back), "occ2")
  expect_equal(as.data.frame(.back), .df)

  ## the default level is still returned bare, as it always has been
  expect_true(inherits(lotri::as.lotri(as.data.frame(lotri::lotri({
    tp <- 1
    a ~ 0.5
  }))), "matrix"))

  ## and a multi level object is unchanged
  expect_equal(names(lotri::as.lotri(as.data.frame(lotri::lotri({
    tp <- 1
    a ~ 0.5
    b ~ 0.6 | occ
  })))), c("id", "occ"))
})

test_that("only the open block follows a condition to its level", {

  ## The rows of one block share a level because they covary, so a
  ## condition written on a continuation carries THAT block over.  It
  ## used to carry the whole default level with it, relocating
  ## parameters declared long before and unrelated to the block.
  .m <- lotri::lotri({
    z1a + z1b + z1c ~ c(1,
                        0.1, 2,
                        0.1, 0.1, 3)
    z2a + z2b ~ c(1.2,
                  0.1, 2.2)
    z3 ~ 1.3 * 1
    z4 ~ c(0.1, 2.4) | occ
  })

  expect_equal(dimnames(unclass(.m$id))[[1]],
               c("z1a", "z1b", "z1c", "z2a", "z2b"))
  expect_equal(dimnames(unclass(.m$occ))[[1]], c("z3", "z4"))

  ## the two-statement shape too
  .s <- lotri::lotri({
    z1 ~ 1.1
    z2 ~ 1.2
    z3 ~ c(0.1, 2.3) | occ
  })
  expect_equal(dimnames(unclass(.s$id))[[1]], "z1")
  expect_equal(dimnames(unclass(.s$occ))[[1]], c("z2", "z3"))

  ## a block opened in the line form and conditioned on its LAST row
  ## still moves whole
  .l <- lotri::lotri({
    f ~ 1
    g ~ c(0.5, 1)
    h ~ c(0.1, 0.2, 2) | occ
    m ~ 2
  })
  expect_equal(dimnames(unclass(.l$occ))[[1]], c("f", "g", "h"))
  expect_equal(dimnames(unclass(.l$id))[[1]], "m")
})

test_that("an unconditioned line is not folded into a stale level", {

  ## a line that cannot be parsed at the default level fell back to the
  ## most recently SEEN condition, even when the block right before it
  ## was at the default level -- so a parameter with no condition landed
  ## at a level of variability it was never given
  expect_error(lotri::lotri({
    z1 ~ 1.1 | occ
    z2 ~ 1.2
    z3a + z3b ~ c(1.3,
                  0.1, 2.3)
    z4 ~ c(0.1, 2.4)
  }), "lotri syntax errors above")

  ## the fold itself is still there when the block before it really is
  ## at that level: an unconditioned continuation joins it
  .m <- lotri::lotri({
    a ~ 1 | occ
    b ~ c(0.1, 2)
  })
  expect_equal(dimnames(unclass(.m$occ))[[1]], c("a", "b"))

  .m2 <- lotri::lotri({
    a ~ 1 | occ
    b ~ c(0.1, 2)
    d ~ c(0.1, 0.2, 3)
  })
  expect_equal(dimnames(unclass(.m2$occ))[[1]], c("a", "b", "d"))
})

test_that("a conditioned continuation stays at the level it names", {

  ## `z4` is written `| occ`, but the block open at `occ` is `{z1, z2}`,
  ## which would need three values to extend.  The line used to be parsed
  ## into the DEFAULT level instead, where it happened to fit as a
  ## continuation of `z3` -- so `| occ` silently produced an id level
  ## (between subject) parameter.  It now carries `z3`'s block over, the
  ## same rule a first mention of the level already used.
  .m <- lotri::lotri({
    z1 ~ 1 | occ
    z2 ~ c(0.1, 2) | occ
    z3 ~ c(1.3)
    z4 ~ c(0.1, 2.4) | occ
  })

  expect_equal(names(.m), "occ")
  expect_equal(dimnames(unclass(.m$occ))[[1]], c("z1", "z2", "z3", "z4"))
  ## the carried block keeps its own values, and does not covary with the
  ## block that was already at the level
  expect_equal(unname(unclass(.m$occ)),
               matrix(c(1, 0.1, 0, 0,
                        0.1, 2, 0, 0,
                        0, 0, 1.3, 0.1,
                        0, 0, 0.1, 2.4), 4, 4))

  ## which is the same matrix the level's first mention gives
  expect_equal(dimnames(unclass(lotri::lotri({
    z3 ~ c(1.3)
    z4 ~ c(0.1, 2.4) | occ
  })$occ))[[1]], c("z3", "z4"))

  ## a trailing label follows it to that level
  .l <- lotri::lotri({
    tp <- 1
    z1 ~ 1 | occ
    z2 ~ c(0.1, 2) | occ
    z3 ~ c(1.3)
    z4 ~ c(0.1, 2.4) | occ
    label("L4")
  })
  expect_equal(attr(.l$occ, "lotriLabels"), c(NA, NA, NA, "L4"))

  ## `same()` repeats the carried block, not the one that was there first
  .s <- lotri::lotri({
    z1 ~ 1 | occ
    z2 ~ c(0.1, 2) | occ
    z3 ~ c(1.3)
    z4 ~ c(0.1, 2.4) | occ
    z5 + z6 ~ same() | occ
  })
  expect_equal(dimnames(unclass(.s$occ))[[1]],
               c("z1", "z2", "z3", "z4", "z5", "z6"))
  expect_equal(unname(unclass(.s$occ))[5:6, 5:6],
               matrix(c(1.3, 0.1, 0.1, 2.4), 2, 2))

  ## the level's OWN open block still wins when it can take the line
  .o <- lotri::lotri({
    z1 ~ 1 | occ
    z3 ~ c(1.3)
    z4 ~ c(0.1, 2.4) | occ
  })
  expect_equal(dimnames(unclass(.o$id))[[1]], "z3")
  expect_equal(dimnames(unclass(.o$occ))[[1]], c("z1", "z4"))

  ## and a line that can be placed at neither is an error rather than a
  ## quiet default level row
  expect_error(lotri::lotri({
    z1 ~ 1 | occ
    z2 ~ c(0.1, 2) | occ
    z3 ~ c(1.3)
    z4 ~ c(0.1, 2.4, 0.5, 0.6, 1) | occ
  }))
})

test_that("naming a level again writes to it rather than replacing it", {

  ## the level was found by "was it the last one seen", so a line at
  ## another level in between made the second mention build a FRESH
  ## environment over the top of it -- silently dropping `z1`
  .m <- lotri::lotri({
    z1 ~ 1 | occ
    z2 ~ 1 | occ2
    z3 ~ c(1.3)
    z4 ~ c(0.1, 2.4) | occ
  })

  expect_equal(dimnames(unclass(.m$occ))[[1]], c("z1", "z4"))
  expect_equal(dimnames(unclass(.m$occ2))[[1]], "z2")
  expect_equal(dimnames(unclass(.m$id))[[1]], "z3")

  ## the properties of the first mention survive the second too
  .p <- lotri::lotri({
    z1 ~ 1 | occ(lower = 0.01)
    z2 ~ 1 | occ2
    z3 ~ 2 | occ
  })
  expect_equal(dimnames(unclass(.p$occ))[[1]], c("z1", "z3"))
  expect_equal(attr(.p, "lotri")$occ$lower, c(z1 = 0.01, z3 = 0.01))
})

test_that("an unconditioned continuation folds into the open block, not the level", {

  ## Folding a line into the level of the line before it went by the
  ## level's whole height, which agreed with the open block only while a
  ## level held one block.  Once `z1` stays at `occ`, that height spans
  ## two blocks and asked for a row count no line could supply.
  .m <- lotri::lotri({
    z1 ~ 1.1 | occ
    z2 ~ 1.2 | occ2
    z3 ~ 1.3 | occ
    z4 ~ c(0.4, 2.4)
  })

  expect_equal(dimnames(unclass(.m$occ))[[1]], c("z1", "z3", "z4"))
  expect_equal(dimnames(unclass(.m$occ2))[[1]], "z2")
  ## `z4` covaries with `z3`, the block it continues -- not with `z1`
  expect_equal(unname(unclass(.m$occ)),
               matrix(c(1.1, 0, 0,
                        0, 1.3, 0.4,
                        0, 0.4, 2.4), 3, 3))

  ## the same shape without the level in between
  .s <- lotri::lotri({
    z1 ~ 1.1 | occ
    z2 ~ 1.2 | occ
    z3 ~ c(0.3, 2.3)
  })
  expect_equal(dimnames(unclass(.s$occ))[[1]], c("z1", "z2", "z3"))
  expect_equal(unname(unclass(.s$occ)),
               matrix(c(1.1, 0, 0,
                        0, 1.2, 0.3,
                        0, 0.3, 2.3), 3, 3))
})
