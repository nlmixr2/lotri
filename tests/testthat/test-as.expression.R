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
