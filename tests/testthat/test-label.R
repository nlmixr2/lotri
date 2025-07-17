test_that("test labels", {

  f <-   lotri({
    # Where initial conditions/variables are specified
    lka  <- log(1.15) ; label("log ka (1/h)")
    lcl  <- log(0.135) ;label("log Cl (L/h)")
    lv   <- log(8)   ;  label("log V (L)")
    prop.err <- 0.15; label("proportional error (SD/mean)")
    add.err  <- 0.6  ; label("additive error (mg/L)")
    eta.ka ~ 0.5   ; label("IIV ka")
    eta.cl ~ 0.1   ; label("IIV cl")
    eta.v  ~ 0.1   ; label("IIV v")
  })

  est <- lotriEst(f)

  w <- which(est$name == "add.err")

  expect_equal(est$label[w], "additive error (mg/L)")

  f <-   lotri({
    # Where initial conditions/variables are specified
    lka  <- log(1.15) ; label("log ka (1/h)")
    lcl  <- log(0.135) ;label("log Cl (L/h)")
    lv   <- log(8)   ;  label("log V (L)")
    prop.err <- 0.15; label("proportional error (SD/mean)")
    add.err  <- 0.6  ; label("additive error (mg/L)")
    eta.ka ~ 0.5   ; label("IIV ka")
    eta.cl ~ c(0.1, 0.1)   ; label("IIV cl")
    eta.v  ~ c(0.1, 0.1, 0.1)   ; label("IIV v")
  })

  f <-   lotri({
    # Where initial conditions/variables are specified
    lka  <- log(1.15) ; label("log ka (1/h)")
    lcl  <- log(0.135) ;label("log Cl (L/h)")
    lv   <- log(8)   ;  label("log V (L)")
    prop.err <- 0.15; label("proportional error (SD/mean)")
    add.err  <- 0.6  ; label("additive error (mg/L)")
    eta.ka + eta.cl ~ c(0.5,
                        0.1, 0.1)   ; label("IIV cl")
                        eta.v  ~ c(0.1)   ; label("IIV v")
  })


})
