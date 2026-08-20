# Tests for as.expression.R coverage: lotriAsExpression, as.expression.lotriFix

test_that("lotriAsExpression returns a call for a lotriFix matrix", {
  m <- lotri({ a + b ~ fix(1, 0.5, 1) })
  expr <- lotriAsExpression(m)
  expect_true(is.call(expr))
})

test_that("lotriAsExpression with plusNames=TRUE uses + syntax", {
  m <- lotri({ a + b ~ fix(1, 0.5, 1) })
  expr <- lotriAsExpression(m, plusNames = TRUE)
  expect_true(is.call(expr))
  txt <- deparse(expr)
  expect_true(any(grepl("[+]", txt)))
})

test_that("lotriAsExpression with logical nameEst=TRUE names all elements", {
  m <- lotri({ a + b ~ fix(1, 0.5, 1) })
  expr <- lotriAsExpression(m, nameEst = TRUE)
  expect_true(is.call(expr))
  txt <- paste(deparse(expr), collapse = "\n")
  # Named elements have "a =" or "a=" in the output (deparse may add spaces)
  expect_true(grepl("a\\s*=", txt))
})

test_that("lotriAsExpression with logical nameEst=FALSE omits names", {
  m <- lotri({ a + b ~ fix(1, 0.5, 1) })
  expr <- lotriAsExpression(m, nameEst = FALSE)
  expect_true(is.call(expr))
})

test_that("as.expression.lotriFix with plusNames=TRUE uses + form", {
  m <- lotri({ a + b ~ fix(1, 0.5, 1) })
  expr <- as.expression(m, plusNames = TRUE)
  expect_true(is.call(expr))
  txt <- deparse(expr)
  expect_true(any(grepl("[+]", txt)))
})

test_that("as.expression.lotriFix with nameEst=TRUE names fixed elements", {
  m <- lotri({ a + b ~ fix(1, 0.5, 1) })
  expr <- as.expression(m, nameEst = TRUE)
  txt <- paste(deparse(expr), collapse = "\n")
  # Fixed elements should appear as fix(...) and be named
  expect_true(grepl("fix", txt))
})

test_that("lotriAsExpression with mixed fixed/non-fixed uses fix() for fixed elements", {
  # Build a matrix where only the off-diagonal is fixed (mixed fix state)
  df <- data.frame(
    ntheta = c(NA_integer_, NA_integer_, NA_integer_),
    neta1 = c(1L, 2L, 2L),
    neta2 = c(1L, 1L, 2L),
    name = c("a", "(a,b)", "b"),
    lower = c(-Inf, -Inf, -Inf),
    est = c(1.0, 0.5, 1.0),
    upper = c(Inf, Inf, Inf),
    fix = c(FALSE, TRUE, FALSE),
    label = c(NA_character_, NA_character_, NA_character_),
    backTransform = c(NA_character_, NA_character_, NA_character_),
    condition = c("id", "id", "id")
  )
  mat <- lotriEst(as.lotri(df), drop = TRUE)
  # nameEst=TRUE: fixed off-diagonal gets named fix() form (line 172-173)
  expr1 <- lotriAsExpression(mat, nameEst = TRUE)
  txt1 <- paste(deparse(expr1), collapse = "\n")
  expect_true(grepl("fix", txt1))
  expect_true(grepl("a\\s*=", txt1))  # named element
  # nameEst=FALSE: fixed off-diagonal gets unnamed fix() form (line 174-175)
  expr2 <- lotriAsExpression(mat, nameEst = FALSE)
  txt2 <- paste(deparse(expr2), collapse = "\n")
  expect_true(grepl("fix", txt2))
})

test_that("named c() form round trips for a combined theta+omega matrix (#53)", {
  # a later row's named c(...) names an earlier, already-closed block --
  # here every `om.` row names all four preceding theta rows -- which
  # used to be misparsed as the `om.` normal prior shorthand instead of
  # a row of the matrix itself
  m <- lotri({
    tka ~ c(tka = 0.0357965110557473)
    tcl ~ c(tka = 4.74020051192728e-05, tcl = -7.11894473061715e-05)
    tv ~ c(tka = 0.000678520687634397, tcl = 4.38111519696731e-05,
           tv = 0.00214721721705466)
    add.sd ~ c(tka = -0.000164787436893072, tcl = 0.000493477395076506,
               tv = 1.7413836554896e-05, add.sd = 0.00241711934195097)
    om.eta.ka ~ c(tka = 0.000287595043694571, tcl = -0.00230271807484326,
                  tv = 0.00015047246393971, add.sd = 6.48357837847193e-05,
                  om.eta.ka = -0.000435491782029363)
    om.eta.cl ~ c(tka = -9.4552153943755e-05, tcl = 4.38078587645025e-05,
                  tv = -5.8098244416014e-05, add.sd = 4.01067877168489e-05,
                  om.eta.ka = 0.000374001386487173, om.eta.cl = 0.00125844682394699)
    om.eta.v ~ c(tka = 6.87088714216441e-05, tcl = -1.52706844989712e-05,
                 tv = 3.53723630641022e-05, add.sd = -3.2063332002243e-05,
                 om.eta.ka = -1.81630195582799e-05, om.eta.cl = -8.81047632309001e-05,
                 om.eta.v = 0.000134094244374112)
  })
  expect_equal(dim(m), c(7L, 7L))
  expect_equal(unname(m), t(unname(m)))

  expr <- lotriAsExpression(m, nameEst = 5L)
  m2 <- eval(expr)
  expect_equal(unname(m), unname(m2))
})
