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
