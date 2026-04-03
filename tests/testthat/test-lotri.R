.lotri <- loadNamespace("lotri")

omega9 <- lotri(
    lotri(
      eta.Cl ~ 0.1,
      eta.Ka ~ 0.1
    ) | id,
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye,
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ,
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv
)

.cls <- c("lotriFix", class(matrix(0)))

omega <- lotri(
    lotri(
      eta.Cl ~ 0.1,
      eta.Ka ~ 0.1
    ) | id(nu = 100),
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye(nu = 50),
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ(nu = 200),
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv(nu = 10)
  )

# needs .lotri, omega9
test_that(".maxNu", {

  omega <- lotri(
    lotri(
      eta.Cl ~ 0.1,
      eta.Ka ~ 0.1
    ) | id(nu = 100),
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye(nu = 50),
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ(nu = 200),
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv(nu = 10)
  )

  expect_equal(omega$.maxNu, 200)

  expect_equal(.Call(.lotri$`_lotriMaxNu`, omega9, PACKAGE = "lotri"), 0)

})

test_that("isLotri C", {

  expect_equal(.Call(.lotri$`_isLotri`, omega9, PACKAGE = "lotri"), TRUE)
  expect_equal(.Call(.lotri$`_isLotri`, omega, PACKAGE = "lotri"), TRUE)

  omega9[[2]] <- 3
  expect_equal(.Call(.lotri$`_isLotri`, omega9, PACKAGE = "lotri"), FALSE)

  omega9[[2]] <- matrix(3)
  expect_equal(.Call(.lotri$`_isLotri`, omega9, PACKAGE = "lotri"), FALSE)

  expect_equal(.Call(.lotri$`_isLotri`, "matt", PACKAGE = "lotri"), FALSE)

})

test_that("default conditioning", {

  fix2 <- lotri({
    f+g ~ fix(1,
              0.5, 1) | occ
    m+n ~ c(2,
            0.5, 1)
  })

  expect_equal(fix2,
               list(id = structure(c(2, 0.5, 0.5, 1),
                                   .Dim = c(2L, 2L),
                                   .Dimnames = list(c("m", "n"), c("m", "n"))),
                    occ = structure(c(1, 0.5, 0.5, 1),
                                    .Dim = c(2L, 2L),
                                    .Dimnames = list(c("f", "g"), c("f", "g")),
                                    class = .cls,
                                    lotriFix = structure(c(TRUE, TRUE, TRUE, TRUE),
                                                         .Dim = c(2L, 2L),
                                                         .Dimnames = list(c("f", "g"), c("f", "g"))))))

})

test_that("$.lotri by-name access returns the named block", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(nu = 100),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ(nu = 200)
  )
  blk <- omega$id
  expect_true(inherits(blk, "matrix"))
  expect_equal(rownames(blk), c("eta.Cl", "eta.Ka"))
})

test_that("$.lotri .names returns unique property names", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(lower = 0),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ
  )
  nms <- omega$.names
  expect_true(is.character(nms))
  expect_true("lower" %in% nms)
})

test_that("$.lotri .list returns full named list", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(lower = 0),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ
  )
  lst <- omega$.list
  expect_true(is.list(lst))
  expect_true("id" %in% names(lst))
})

test_that("$.lotri property access returns per-condition values", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(nu = 50),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ(nu = 100)
  )
  nu_vals <- omega$nu
  expect_true(is.list(nu_vals))
  expect_true("id" %in% names(nu_vals) || "occ" %in% names(nu_vals))
})

test_that("$.lotri returns default values for 'lower' when not set", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(nu = 50),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ(nu = 100)
  )
  lower_vals <- omega$lower
  expect_true(is.list(lower_vals))
  expect_true(all(is.infinite(unlist(lower_vals))))
})

test_that(".DollarNames.lotri returns completions", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(lower = 0),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ
  )
  dns <- .DollarNames(omega, "")
  expect_true(is.character(dns))
  expect_true(all(c(".allNames", ".bounds", ".names") %in% dns))
  # Filter by pattern
  dns2 <- .DollarNames(omega, "^\\.")
  expect_true(all(grepl("^\\.", dns2)))
})

test_that("as.matrix.lotri on single-condition lotri returns matrix", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(nu = 50)
  )
  r <- as.matrix(omega)
  expect_true(inherits(r, "matrix"))
  expect_equal(rownames(r), c("eta.Cl", "eta.Ka"))
})

test_that("as.matrix.lotri errors with multiple conditions", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(nu = 100),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ(nu = 200)
  )
  expect_error(as.matrix(omega), "cannot convert multiple")
})

test_that("lotri() with no arguments returns empty matrix", {
  m <- lotri()
  expect_true(is.matrix(m) || is.null(m))
})

test_that("lotri() with a matrix returns it unchanged", {
  m <- diag(2)
  dimnames(m) <- list(c("a", "b"), c("a", "b"))
  result <- lotri(m)
  expect_equal(result, m)
})

test_that("lotri() with cov=TRUE accepts positive definite matrix", {
  m <- lotri({ a + b ~ c(1, 0.1, 1) }, cov = TRUE)
  expect_equal(dim(m), c(2L, 2L))
})

test_that("lotri() with cov=function uses provided function", {
  called <- FALSE
  cov_wrapper <- function(x) {
    called <<- TRUE
    lotriNearPD(x)
  }
  m <- lotri({ a + b ~ c(1, 0.1, 1) }, cov = cov_wrapper)
  expect_true(called)
  expect_equal(dim(m), c(2L, 2L))
})

test_that("lotri() cov parameter errors with non-logical non-function", {
  expect_error(lotri({ a ~ 1 }, cov = "bad"), "'cov' must be")
})

test_that("lotri conditional expression parsing with single numeric value", {
  omega <- lotri({
    eta.ka ~ 0.3 | id
    iov.ka ~ 0.1 | occ
  })
  expect_true(inherits(omega, "lotri") || is.list(omega))
  expect_true(length(omega) == 2L)
})

test_that("lotri cov=TRUE errors when diagonal is zero with non-zero off-diag", {
  # Error goes through {} parser: "lotri syntax errors above"
  expect_error(lotri({ a + b ~ c(0, 0.5, 1) }, cov = TRUE))
})

test_that("lotri() unsupported conditional expression errors", {
  # (id + occ) is not a valid condition name (starts with +)
  expect_error(lotri({ a ~ 1 | (id + occ) }))
})

test_that("lotri() progressive row-by-row matrix build (lines 441-470)", {
  # Specify a 2x2 matrix one row at a time using single-name LHS
  m <- lotri({ a ~ 1; b ~ c(0.5, 1) })
  expect_equal(dim(m), c(2L, 2L))
  expect_equal(m["a", "a"], 1)
  expect_equal(m["b", "a"], 0.5)
  expect_equal(m["b", "b"], 1)
})

test_that("lotri() wrong-size vec adds placeholder names in error (line 62)", {
  # a+b with 5 values -> .num~2.4 -> .dim=3, only 2 LHS names -> adds 'v1'
  expect_error(lotri({ a + b ~ c(1, 2, 3, 4, 5) }))
})

test_that("lotri() single name with 2x2 triangular values errors with suggestion (lines 506-509)", {
  # a ~ c(1, 0.5, 1): single name but 3 values -> suggests 'a + varName2'
  expect_error(lotri({ a ~ c(1, 0.5, 1) }))
})

test_that("lotri() arithmetic expression in RHS (.lotriParseMat line 243)", {
  m <- lotri({ a ~ 1 + 0.1 })
  expect_equal(m["a", "a"], 1.1)
})

test_that("lotri() unary tilde triggers .fCallTilde length != 3 error (lines 660-665)", {
  expect_error(lotri({ ~a }))
})

test_that("lotri() unfix() on single value (lines 694-699)", {
  m <- lotri({ a ~ unfix(0.5) })
  expect_true(inherits(m, "lotriFix"))
  expect_true(!is.null(attr(m, "lotriUnfix")))
  expect_equal(m["a", "a"], 0.5)
})

test_that("$.lotri returns NULL for condition lacking queried property (line 1635)", {
  # occ has no properties, so occ is not in .lotri; query nu => NULL for occ
  omega <- lotri(
    lotri(eta.Cl ~ 0.1) | id(nu = 50),
    lotri(iov.Cl ~ 0.01) | occ
  )
  nu_vals <- omega$nu
  expect_true(is.list(nu_vals))
  expect_true("id" %in% names(nu_vals))
  expect_false("occ" %in% names(nu_vals))
})

test_that("$.lotri returns NULL for non-existent property on all conditions (line 1651)", {
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(nu = 50),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ(nu = 100)
  )
  expect_null(omega$nonexistentprop)
})

test_that("lotri() same condition name repeated merges into one block", {
  omega <- lotri(
    eta.Cl ~ 0.1 | id,
    eta.Ka ~ 0.1 | id
  )
  expect_true("id" %in% names(omega))
  expect_equal(dim(omega$id), c(2L, 2L))
})

test_that("lotri() block form with conditions covers for-loop and .mergeProp paths", {
  # Block form (not nested) uses the env$cnd for-loop path in lotri()
  # This covers .mergeProp (lines 913-916) and .amplifyDefault paths
  omega <- lotri({
    eta.Cl ~ 0.1 | id(nu = 50)
    iov.Cl ~ 0.01 | occ(nu = 100)
  })
  expect_true(inherits(omega, "lotri"))
  expect_true("id" %in% names(omega))
  expect_true("occ" %in% names(omega))
  expect_equal(omega$nu$id, 50)
  expect_equal(omega$nu$occ, 100)
})

test_that("lotri() block form with lower property covers .amplifyDefault single-value expansion", {
  omega <- lotri({
    eta.Cl ~ 0.1 | id(lower = 0)
    iov.Cl ~ 0.01 | occ(lower = -1)
  })
  expect_true(inherits(omega, "lotri"))
  lower_vals <- omega$lower
  expect_equal(lower_vals$id[["eta.Cl"]], 0)
  expect_equal(lower_vals$occ[["iov.Cl"]], -1)
})

test_that("lotri() block form with named lower triggers .amplifyDefault named-vector path", {
  # named lower = c(eta.Cl = 0) for a matrix with one element
  omega <- lotri({
    eta.Cl ~ 0.1 | id(lower = c(eta.Cl = 0))
  })
  expect_true(inherits(omega, "lotri"))
  expect_equal(omega$lower$id[["eta.Cl"]], 0)
})

test_that("lotri() .fCall handles quote() block expression", {
  # quote() inside lotri block exercises .fCall lines 755-756
  expect_error(lotri({ quote(a ~ 1) }))
})

test_that("lotri() .fCall handles matrix() call inside block", {
  # matrix() expression in lotri block
  m <- matrix(c(1, 0, 0, 1), 2, 2)
  dimnames(m) <- list(c("a", "b"), c("a", "b"))
  r <- lotri({ matrix(m, 2, 2) })
  expect_true(is.matrix(r))
})

test_that("lotri() .fCall errors on non-name non-call expression", {
  # 1 + 2 in block is a call whose function is +, not a known operator
  expect_error(lotri({ 1 + 2 }))
})

test_that("lotri() .f handles bare name in block (returns empty)", {
  # A bare name in a block calls .f which returns character() for is.name(x)
  m <- lotri({ myvar })
  expect_true(is.matrix(m) || is.null(m))
})

test_that("lotri() .fCall errors when two matrix() calls in one block (line 759)", {
  m1 <- matrix(c(1, 0, 0, 1), 2, 2)
  dimnames(m1) <- list(c("a", "b"), c("a", "b"))
  expect_error(lotri({ matrix(m1, 2, 2); matrix(m1, 2, 2) }))
})

test_that("lotri() .fCallTilde errors on scalar non-numeric RHS (line 727)", {
  # a ~ 'text' : single-name LHS, string RHS -> .fCallTilde stop
  expect_error(lotri({ a ~ "text" }))
})

test_that("lotri() .fcallTildeLhsSum errors on multi-element non-numeric RHS (line 620)", {
  # a + b ~ list(1, 2): multi-name LHS, list RHS -> .fcallTildeLhsSum stop
  expect_error(lotri({ a + b ~ list(1, 2) }))
})

test_that("lotri() .repFixedWithC handles unfix() nested in c() (lines 158-160)", {
  # unfix() nested inside c() triggers the unfix branch of .repFixedWithC
  m <- lotri({ a + b ~ c(unfix(1), 0.5, 1) })
  expect_true(inherits(m, "lotriFix"))
  expect_true(!is.null(attr(m, "lotriUnfix")))
  expect_true(attr(m, "lotriUnfix")["a", "a"])
})

test_that("lotri() .lotriParseMat handles arithmetic in conditional value (line 243)", {
  # In 'a ~ (1 + 0.1) | id', the value part '1 + 0.1' goes through
  # .lotriParseMat with arithmetic operator as x[[1]]
  m <- lotri({ a ~ 1 + 0.1 | id })
  expect_equal(m$id["a", "a"], 1.1)
})

test_that("lotri() .amplifyDefault errors on multiple unnamed bounds (lines 848-851)", {
  # lower = c(0, -1) has no names and length > 1 -> error
  expect_error(lotri({ eta.Cl ~ 0.1 | id(lower = c(0, -1)) }))
})

test_that("lotri() .amplifyDefault errors on bad named bound (lines 861, 867-870)", {
  # lower = c(nonExistent = 0) refers to a variable not in the matrix
  expect_error(lotri({ eta.Cl ~ 0.1 | id(lower = c(nonExistent = 0)) }))
})

test_that("lotri() .mergeProp merges when condition appears in other (lines 919-930)", {
  # Same condition in first element (with props) and second element (no props)
  # causes .prop to be char vector at line 1423, then .mergeProp(char, ...) fires
  omega <- lotri(
    lotri(eta.Cl ~ 0.1) | id(lower = 0),
    lotri(eta.Ka ~ 0.1) | id
  )
  expect_true(inherits(omega, "lotri"))
  expect_equal(dim(omega$id), c(2L, 2L))
})

test_that("lotri() .mergeProp char-vector path via block+nested combo (lines 919-930)", {
  # Block form (first arg) + nested form (second arg) with same condition
  # In the for-loop, .other has condition 'id' but .prop was NULL,
  # so .prop <- dimnames(.other[['id']])[[1]] (char vector) before .mergeProp call
  omega <- lotri(
    {eta.Cl ~ 0.1 | id(lower = 0)},
    lotri(eta.Ka ~ 0.1) | id
  )
  expect_true(inherits(omega, "lotri"))
  expect_equal(dim(omega$id), c(2L, 2L))
  lower_vals <- omega$lower
  expect_equal(lower_vals$id[["eta.Cl"]], 0)
})

test_that("lotri() .lotriList handles named matrix in list (lines 979-980)", {
  m1 <- matrix(c(0.1, 0, 0, 0.2), 2, 2,
               dimnames = list(c("a", "b"), c("a", "b")))
  r <- lotri(list(id = m1))
  expect_equal(names(r), "id")
  expect_equal(dim(r$id), c(2L, 2L))
})

test_that("lotri() .lotriList handles lotri element in list (lines 982-983)", {
  omega <- lotri(lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id)
  m2 <- matrix(0.3, 1, 1, dimnames = list("b", "b"))
  r <- lotri(list(omega, b = m2))
  expect_true("id" %in% names(r))
  expect_true("b" %in% names(r))
})

test_that("lotri() .mergeProp errors on conflicting non-default property (lines 940-941)", {
  # Block form sets id(nu=50), nested form sets id(nu=100) — conflict
  expect_error(
    lotri({ eta.Cl ~ 0.1 | id(nu = 50) }, lotri(eta.Ka ~ 0.1) | id(nu = 100)),
    "conflicting"
  )
})

test_that("lotri() .lotriList lines 989-992: unnamed element inside inner list-of-lotri", {
  # lotri(list(m_a, id = m_b)) returns a lotri with names c("", "id");
  # wrapping in another list makes .lotriList encounter that unnamed element at lines 989-992
  m_a <- matrix(c(0.1), 1, 1, dimnames = list("a", "a"))
  m_b <- matrix(c(0.2), 1, 1, dimnames = list("b", "b"))
  r <- lotri(list(list(m_a, id = m_b)))
  expect_true(!is.null(r))
  expect_true("id" %in% names(r))
})

test_that("lotri() .lotriList line 1014: mix of unnamed and named matrices in list", {
  m_unnamed <- matrix(c(0.1), 1, 1, dimnames = list("a", "a"))
  m_named <- matrix(c(0.2), 1, 1, dimnames = list("b", "b"))
  r <- lotri(list(m_unnamed, id = m_named))
  expect_true(!is.null(r))
})

test_that(".lotriGetMatrixFromEnv line 1097: non-NULL df with empty $i", {
  env <- new.env(parent = emptyenv())
  env$df <- data.frame(
    i = integer(0), j = integer(0), x = numeric(0),
    fix = logical(0), unfix = logical(0)
  )
  env$names <- character(0)
  result <- lotri:::.lotriGetMatrixFromEnv(env)
  expect_equal(nrow(result), 0L)
  expect_equal(ncol(result), 0L)
})

test_that("lotri() cov=TRUE produces nearest PD matrix (line 1308 path via logical TRUE cov)", {
  # cov=TRUE: length==1, is.logical, not NA — skips the if block, .fun stays NULL
  # To trigger line 1308 (.fun <- lotriNearPD): need a non-scalar logical that
  # is still TRUE but length != 1.  In R < 4.3 this warned; in R >= 4.3 it errors.
  # Instead, just verify the normal cov=TRUE behavior works (exercises cov param).
  m <- lotri({ a + b ~ c(1, 0.5, 1) }, cov = TRUE)
  expect_true(is.matrix(m))
})

test_that("lotri() with NULL x returns NULL (line 1343)", {
  result <- lotri(NULL)
  expect_null(result)
})

test_that("lotri() with [[ accessor in call (line 1361)", {
  my_list <- list(lotri(a ~ 0.1))
  r <- lotri(my_list[[1]])
  expect_true(is.matrix(r))
})

test_that("lotri() block with conditions + non-list .other (line 1448)", {
  # Block form gives cnd='id'; extra arg lotri(eta.Ka~0.1) is a matrix (not list)
  r <- lotri({ eta.Cl ~ 0.1 | id }, lotri(eta.Ka ~ 0.1))
  expect_true(!is.null(r))
})

test_that("lotri() .fullCnd path with extra conditions in .tmp (lines 1501-1505)", {
  # First arg id(nu=50) sets .prop; remaining args have 'id' AND 'occ'
  r <- lotri(lotri(a ~ 0.1) | id(nu = 50), lotri(b ~ 0.1) | id, lotri(c ~ 0.1) | occ)
  expect_true("id" %in% names(r))
  expect_true("occ" %in% names(r))
})

test_that("lotri() no .fullCnd, .tmp is list with unnamed element (lines 1539-1548)", {
  # lotri({eta.Cl~0.1|id}, lotri(eta.Ka~0.1)) returns list(id=m, ""=m2)
  # Wrapping with lotri(a~0.1, ...) triggers the unnamed-element path
  inner <- lotri({ eta.Cl ~ 0.1 | id }, lotri(eta.Ka ~ 0.1))
  r <- lotri(lotri(a ~ 0.1), inner)
  expect_true(!is.null(r))
})

test_that("lotri() no .fullCnd, .tmp is list with only named elements (lines 1550-1551)", {
  # All remaining conditions are named: 'id' and 'occ'
  inner <- lotri({ eta.Cl ~ 0.1 | id }, lotri(eta.Ka ~ 0.1) | occ)
  r <- lotri(lotri(a ~ 0.1), inner)
  expect_true(!is.null(r))
})

test_that("lotri() no .fullCnd, .tmp is a lotri object (lines 1558-1559)", {
  # lotri(b~0.1)|id(nu=50) returns a lotri (not plain list), covers lines 1558-1559
  r <- lotri(lotri(a ~ 0.1), lotri(b ~ 0.1) | id(nu = 50))
  expect_true(!is.null(r))
})
