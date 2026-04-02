# Tests for fixes to thread safety, memory allocation, and integer type issues.
#
# Issues addressed:
#   Bug 1: Variable shadowing in lotriProp.c:94 (double val shadows parameter val)
#   Bug 2: uword underflow in rcm.cpp (n=0 causes n-1 to wrap to UWORD_MAX)
#   Bug 3: Type narrowing in nearPD.cpp (unsigned int n from arma::uword + n=0 guard)
#   Issue 4: Hard-coded buffer size in matlist.h (char out[100] -> out[256])
#   Issue 5: int stores of Rf_length() returns (should be R_xlen_t)

test_that("Bug 1 (variable shadowing): ampDefault fills unnamed single bound to all dims", {
  # ampDefault had `double val = REAL(cur)[0]` shadowing the parameter `val`
  # (R_NegInf / R_PosInf default). After the rename to `inVal`, the single
  # unnamed scalar must still fill all dimensions, while the default must still
  # apply for names not found in the named case.
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(lower = 0),
    lotri(eye.Cl ~ 0.05, eye.Ka ~ 0.05) | eye(upper = 1)
  )
  bounds <- omega$.bounds

  # Single unnamed lower = 0 must expand to both eta.Cl and eta.Ka
  expect_equal(bounds$lower[c("eta.Cl", "eta.Ka")], c(eta.Cl = 0, eta.Ka = 0))

  # Single unnamed upper = 1 must expand to both eye.Cl and eye.Ka
  expect_equal(bounds$upper[c("eye.Cl", "eye.Ka")], c(eye.Cl = 1, eye.Ka = 1))

  # Default upper for id parameters is +Inf (parameter val, not inVal)
  expect_true(all(is.infinite(bounds$upper[c("eta.Cl", "eta.Ka")])))
  expect_true(all(bounds$upper[c("eta.Cl", "eta.Ka")] > 0))

  # Default lower for eye parameters is -Inf
  expect_true(all(is.infinite(bounds$lower[c("eye.Cl", "eye.Ka")])))
  expect_true(all(bounds$lower[c("eye.Cl", "eye.Ka")] < 0))
})

test_that("Bug 1 (variable shadowing): named bounds use default for missing names", {
  # The outer `val` parameter (default) must still apply for names not matched.
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(lower = c(eta.Cl = 0))
  )
  bounds <- omega$.bounds

  # eta.Cl has explicit lower = 0
  expect_equal(bounds$lower[["eta.Cl"]], 0)
  # eta.Ka was not named in lower, so it gets the default R_NegInf
  expect_equal(bounds$lower[["eta.Ka"]], -Inf)
})

test_that("Bug 2 (rcm n=0 underflow): rcm on a 1x1 matrix returns itself", {
  # n=0 would cause n-1 to wrap to UWORD_MAX causing an infinite loop.
  # n=1: the main loop `for (uword i = 0; i < n-1; i++)` evaluates `i < 0`
  # (unsigned), which is immediately false, so the loop body never executes.
  # The result is the same 1x1 matrix, which is correct.
  m1 <- matrix(3.14, nrow = 1, ncol = 1, dimnames = list("a", "a"))
  result <- rcm(m1)
  expect_equal(result, m1)
  expect_equal(dimnames(result), list("a", "a"))
})

test_that("Bug 3 (nearPD n=0 guard): nearPD on a 1x1 positive-definite matrix", {
  # An n=0 guard was added to lotriNearPDarma: without it, d(n-1) accesses
  # d[UINT_MAX] when n=0. For n=1, the algorithm should converge immediately
  # because a 1x1 positive scalar is already positive definite.
  m1 <- matrix(4.0, nrow = 1, ncol = 1)
  result <- lotriNearPD(m1)
  expect_equal(result, m1)

  # Also verify dimnames are preserved for n=1
  dimnames(m1) <- list("x", "x")
  result2 <- lotriNearPD(m1)
  expect_equal(dimnames(result2), list("x", "x"))
})

test_that("Issue 5 (R_xlen_t types): multi-block lotriMat assembles correctly", {
  # Exercises the lstLen / inLen / nDim / nnames code paths now using R_xlen_t.
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id,
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ
  )
  m <- lotriMat(omega)
  expect_equal(dim(m), c(4L, 4L))
  expect_equal(rownames(m), c("eta.Cl", "eta.Ka", "iov.Cl", "iov.Ka"))
  expect_equal(m["eta.Cl", "eta.Cl"], 0.1)
  expect_equal(m["iov.Ka", "iov.Ka"], 0.01)
  # Off-diagonal block must be zero (block-diagonal structure)
  expect_equal(m["eta.Cl", "iov.Cl"], 0.0)
  expect_equal(m["eta.Ka", "iov.Ka"], 0.0)
})

test_that("Issue 4 (buffer size): setStrElt formatted names are not truncated", {
  # char out[256] with sizeof(out) replaces the old char out[100] with
  # hard-coded 100. Verify formatted names via lotriSep produce correct output.
  # Uses the same 4-group omega pattern as the existing bounds test.
  omega <- lotri(
    lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(nu = 100, lower = 3, upper = 4),
    lotri(eye.Cl ~ 0.05, eye.Ka ~ 0.05) | eye(nu = 50, lower = c(eye.Cl = 4)),
    lotri(iov.Cl ~ 0.01, iov.Ka ~ 0.01) | occ(nu = 200),
    lotri(inv.Cl ~ 0.02, inv.Ka ~ 0.02) | inv(nu = 10)
  )
  sepA <- lotriSep(omega, above = c(inv = 10L), below = c(eye = 2L, occ = 4L))

  above_names <- rownames(lotriMat(sepA$above))
  # inv repeated 10 times, 2 params = THETA[1]...THETA[20]
  expect_equal(above_names, sprintf("THETA[%d]", 1:20))

  below_names <- rownames(lotriMat(sepA$below))
  # id block (eta.Cl, eta.Ka) + eye repeated 2x + occ repeated 4x = 12 ETAs
  expect_equal(below_names, c("eta.Cl", "eta.Ka", sprintf("ETA[%d]", 1:12)))
})

test_that("large matrix allocation >2GB is skipped with explanation", {
  # _lotriLstToMat allocates an N x N double matrix using Rf_allocMatrix.
  # Memory required: N^2 * sizeof(double) bytes.
  # For N = 16384: 16384^2 * 8 = 2,147,483,648 bytes = exactly 2 GiB.
  # For N = 16385: 16385^2 * 8 = 2,147,745,800 bytes > 2 GiB.
  #
  # This test is skipped unconditionally because most CI/test environments
  # do not have sufficient RAM for a 2 GiB single allocation, and R itself
  # may refuse to allocate objects this large depending on available memory.
  skip("Requires >2GB RAM: a 16384x16384 double matrix needs 2,147,483,648 bytes (2 GiB). Run manually on a machine with >4GB free RAM.")

  # The actual test (runs only when skip() is removed):
  n_params <- 16384L
  # Build a list of n_params named 1x1 identity matrices
  block_list <- lapply(seq_len(n_params), function(i) {
    nm <- paste0("p", i)
    m <- matrix(1.0, nrow = 1, ncol = 1, dimnames = list(nm, nm))
    m
  })
  # Call _lotriLstToMat directly: allocates a full n_params x n_params dense
  # matrix (~2 GiB). Uses .Call because lotri() uses NSE not suited for lists.
  .lotri <- asNamespace("lotri")
  big_mat <- .Call(.lotri$`_lotriLstToMat`, block_list, NULL, 1L, "matrix")
  expect_equal(nrow(big_mat), n_params)
  expect_equal(ncol(big_mat), n_params)
  # Diagonal should be all 1s
  expect_equal(diag(big_mat), rep(1.0, n_params))
  # Off-diagonal should be all 0s (block diagonal)
  expect_equal(big_mat[1, 2], 0.0)
})
