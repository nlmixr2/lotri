skip_on_cran()

test_that("as.lotri", {
  tmp2 <- lotri(
    iov.Ka ~ 0.5,
    iov.Cl ~ 0.6
  )

  tmp3 <- as.lotri(tmp2)

  expect_equal(tmp3, structure(list(structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
    c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl")
  ))), .Names = "", class = "lotri"))

  tmp3 <- as.lotri(tmp3, default = "id")

  expect_equal(tmp3, structure(list(id = structure(c(0.5, 0, 0, 0.6), .Dim = c(
    2L,
    2L
  ), .Dimnames = list(c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl")))), class = "lotri"))

  expect_true(inherits(as.matrix(tmp3), "matrix"))

  tmp2 <- lotri(
    lotri(
      iov.Ka ~ 0.5,
      iov.Cl ~ 0.6
    ) | iov(lower = 3),
    lotri(
      occ.Ka ~ 0.5,
      occ.Cl ~ 0.6
    )
  )

  expect_error(as.matrix(tmp2))

  l1 <- as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), lower = 4, default = "id")

  l2 <- lotri(et1 + et2 ~ c(0.1, 0.01, 1) | id(lower = 4))

  expect_equal(l1, l2)

  l1 <- as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), nu = 4, default = "id")
  l2 <- lotri(et1 + et2 ~ c(0.1, 0.01, 1) | id(nu = 4))

  expect_equal(l1, l2)

  l1 <- as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = c(et1 = 3), default = "id")
  l2 <- lotri(et1 + et2 ~ c(0.1, 0.01, 1) | id(upper = c(et1 = 3)))

  expect_equal(l1, l2)


  l1 <- as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = c(et1 = 3), matt = NULL, default = "id")
  l2 <- lotri(et1 + et2 ~ c(0.1, 0.01, 1) | id(upper = c(et1 = 3)))

  expect_equal(l1, l2)

  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = c(3, 3), default = "id"))
  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = 1L, default = "id"))

  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), lower = c(3, 3), default = "id"))
  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), lower = 1L, default = "id"))

  expect_error(as.lotri("matt"))

  df <- structure(list(ntheta = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_), neta1 = c(1, 2, 4, 5, 6), neta2 = c(1, 2, 4, 5, 6), name = c("eta.cl", "eta.v", "eta.ec50", "eta.kout", "eta.e0"), lower = c(-Inf, -Inf, -Inf, -Inf, -Inf), est = c(2, 1, 0.5, 0.5, 0.5), upper = c(Inf, Inf, Inf, Inf, Inf), fix = c(FALSE, FALSE, FALSE, FALSE, FALSE), label = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), backTransform = c(NA_character_, NA_character_, NA_character_,  NA_character_, NA_character_), condition = c("id", "id", "id", "id", "id"), err = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)), row.names = c(12L, 13L, 15L, 16L, 17L), class = "data.frame")

  expect_error(as.lotri(df), NA)

})

test_that("no mangling of etas #",{
  
  m <- lotri({
    Ktr_pop = 0.1
    MTT_pop = 0.2
    KA_pop = 0.3
    CL_pop = 0.4
    V1_pop = 0.5
    beta_WT_V1 = 0.6
    WT = 70
    DRUG = 2
    TLAG_pop = 0
    Q_pop = 0
    V2_pop = 1
    FU_pop = 1
    PLTZ_pop = 0.7
    MMT_pop = 0.8
    SPW_pop = 0.9
    EMAX_PLT_pop = 1.0
    EC50_PLT_pop = 0
    SG_pop = 1.1
    KOUT_pop = 1.2
    GDFZ_pop = 1.3
    KIN_pop = 1.4
    KPRO_pop = -1
    KP5_pop = 1
    KP1_pop = 0
    TRTPLT = 0
    TRTGDF = 0
    SLPD_pop = 0
    SLPI_pop = 0
    KE0_pop = 0
    SEP_pop = 0
    CFR_pop = 0
    LPW_pop = 0
    etaKtr ~ sd(0.1)
    etaMTT ~ sd(0.2)
    etaKA ~  sd(0.3)
    etaV1 + etaCL ~ sd(cor(0.4,
                           0.5, 0.6))
    etaPLTZ ~ sd(0.7)
    etaMMT  ~ sd(0.8)
    etaSPW  ~ sd(0.9)
    etaEMAX ~ sd(1.1)
    etaV2 ~ 1e-8
    etaQ ~ 1e-8
    etaKE0 ~ 1e-8
    etaSLPD ~ 1e-8
    etaSLPI ~ 1e-8
    etaCFR  ~ 1e-8
    etaLPW  ~ 1e-8
    etaEC50 ~ 1e-8
    etaKPRO ~ 1e-8
    etaKP5 ~ 1e-8
    etaSG   ~ sd(1.1)
    etaKOUT ~ sd(1.2)
    etaGDFZ ~ sd(1.3)
    etaKIN  ~ sd(1.4)
  })

  expect_equal(m["etaMTT", "etaMTT"], 0.2^2)

  m2 <- as.lotri(as.data.frame(m))

  expect_equal(m, m2)

})
