test_that("as.expression handling; lhs of theta parameters", {

  x <- lotri({
    tka <- 0.45; label("Log Ka")
    tcl <- 1; label("Log Cl")
    tv <- 3.45; label("Log V")
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.err <- 0.7
  })

  xdf <- as.data.frame(x)
  x1 <- xdf[1, ]

  expect_equal(.lotriLhsExpressionFromDf1(x1), quote(0.45))

  x1$fix <- TRUE
  expect_equal(.lotriLhsExpressionFromDf1(x1),quote(fix(0.45)))

  x1$upper <- 1
  expect_equal(.lotriLhsExpressionFromDf1(x1), quote(fix(-Inf, 0.45, 1)))

  x1$fix <- FALSE
  expect_equal(.lotriLhsExpressionFromDf1(x1), quote(c(-Inf, 0.45, 1)))

  x1$upper <- Inf
  x1$lower <- 0
  expect_equal(.lotriLhsExpressionFromDf1(x1), quote(c(0, 0.45)))

  x1$fix <- TRUE
  expect_equal(.lotriLhsExpressionFromDf1(x1), quote(fix(0, 0.45)))

  x1$upper <- 1
  expect_equal(.lotriLhsExpressionFromDf1(x1), quote(fix(0, 0.45, 1)))
  expect_equal(.lotriAssignmentExpressionFromDf1(x1), quote(tka <- fix(0, 0.45, 1)))

  x1$fix <- FALSE
  expect_equal(.lotriLhsExpressionFromDf1(x1), quote(c(0, 0.45, 1)))
  expect_equal(.lotriAssignmentExpressionFromDf1(x1), quote(tka <- c(0, 0.45, 1)))

  expect_equal(.lotriBackTransformFromDf1(x1), NULL)

  expect_equal(.lotriExpressionLinesFromDf1(x1),
               list(quote(tka <- c(0, 0.45, 1)),
                    quote(label("Log Ka"))))

  x1$backTransform <- "exp"
  expect_equal(.lotriBackTransformFromDf1(x1), list(quote(backTransform("exp"))))

  expect_equal(.lotriExpressionLinesFromDf1(x1),
               list(quote(tka <- c(0, 0.45, 1)),
                    quote(backTransform("exp")),
                    quote(label("Log Ka"))))

  expect_equal(.lotriLabelFromDf1(x1), list(quote(label("Log Ka"))))


  x1$label <- "This is a fun label\"'"
  expect_equal(.lotriLabelFromDf1(x1), list(quote(label("This is a fun label\"'"))))

  x1$label <- NA_character_
  expect_equal(.lotriLabelFromDf1(x1), NULL)

  expect_equal(.lotriExpressionLinesFromDf1(x1),
               list(quote(tka <- c(0, 0.45, 1)),
                    quote(backTransform("exp"))))

  x1$backTransform <- NA_character_
  expect_equal(.lotriExpressionLinesFromDf1(x1),
               list(quote(tka <- c(0, 0.45, 1))))

  expect_equal(.lotriGetPopLinesFromDf(xdf),
               list(
                 quote(tka <- 0.45),
                 quote(label("Log Ka")),
                 quote(tcl <- 1),
                 quote(label("Log Cl")),
                 quote(tv <- 3.45),
                 quote(label("Log V")),
                 quote(add.err <- 0.7)))

  x1 <- lotri({
    et5 ~ 1
    et2 + et3 ~ c(
      1,
      2, 3
    )
    et1 ~ fix(3)
  })

  expect_equal(.lotriGetEtaMatrixElementsPlusForm(x1),
               list(quote(et5 ~ 1),
                    quote(et2 + et3 ~ c(1, 2, 3)),
                    quote(et1 ~ fix(3))))

  x1 <- lotri({
    et5 ~ 1; label("eta 5")
    et2 + et3 ~ c(
      1,
      2, 3
    ); label("eta 3")
    et1 ~ fix(3); label("eta 1")
  })

  expect_equal(.lotriGetEtaMatrixElementsLineForm(x1),
               list(quote(et5 ~ 1),
                    quote(label("eta 5")),
                    quote(et2 ~ 1),
                    quote(et3 ~ c(2, 3)),
                    quote(label("eta 3")),
                    quote(et1 ~ fix(3)),
                    quote(label("eta 1"))))

  fix2 <- lotri({
    f+g ~ fix(1,
              0.5, 1) | occ
    m+n ~ c(2,
            0.5, 1)
  })

  expect_equal(.lotriGetEtaMatrixElementsPlusForm(fix2),
               list(quote(m + n ~ c(2, 0.5, 1)),
                    quote(f + g ~ fix(1, 0.5, 1) | occ)))

  expect_equal(.lotriGetEtaMatrixElementsLineForm(fix2),
               list(quote(m ~ 2),
                    quote(n ~ c(0.5, 1)),
                    quote(f ~ fix(1) | occ),
                    quote(g ~ fix(0.5, 1) | occ)))

  fix2 <- lotri({
    f+g ~ fix(1,
              0.5, 1) | occ ; label("g")
    m+n ~ c(2,
            0.5, 1)  ; label("n")
  })

  expect_equal(.lotriGetEtaMatrixElementsLineForm(fix2),
               list(quote(m ~ 2),
                    quote(n ~ c(0.5, 1)),
                    quote(label("n")),
                    quote(f ~ fix(1) | occ),
                    quote(g ~ fix(0.5, 1) | occ),
                    quote(label("g"))))


  x <- lotri({
    tka <- 0.45; label("Log Ka")
    tcl <- 1; label("Log Cl")
    tv <- 3.45; label("Log V")
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.err <- 0.7
  })

  df <- as.data.frame(x)

  expect_equal(lotriDataFrameToLotriExpression(df),
               quote(lotri({
                 tka <- 0.45
                 label("Log Ka")
                 tcl <- 1
                 label("Log Cl")
                 tv <- 3.45
                 label("Log V")
                 add.err <- 0.7
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
               })))

  expect_equal(lotriDataFrameToLotriExpression(df, useIni=TRUE),
               quote(ini({
                 tka <- 0.45
                 label("Log Ka")
                 tcl <- 1
                 label("Log Cl")
                 tv <- 3.45
                 label("Log V")
                 add.err <- 0.7
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
               })))

  # You may also call as.expression directly from the lotri object

  expect_equal(as.expression(x),
               quote(lotri({
                 tka <- 0.45
                 label("Log Ka")
                 tcl <- 1
                 label("Log Cl")
                 tv <- 3.45
                 label("Log V")
                 add.err <- 0.7
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
               })))

  expect_equal(as.expression(x, useIni=TRUE),
               quote(ini({
                 tka <- 0.45
                 label("Log Ka")
                 tcl <- 1
                 label("Log Cl")
                 tv <- 3.45
                 label("Log V")
                 add.err <- 0.7
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
               })))

  x <- lotri({
    tka <- 0.45; label("Log Ka")
    tcl <- 1; label("Log Cl")
    tv <- 3.45; label("Log V")
    eta.ka ~ 0.6 ; label("eta ka")
    eta.cl ~ 0.3 ; label("eta cl")
    eta.v ~ 0.1 ; label("eta v")
    add.err <- 0.7
  })

  df <- as.data.frame(x)

  expect_equal(lotriDataFrameToLotriExpression(df),
               quote(lotri({
                 tka <- 0.45
                 label("Log Ka")
                 tcl <- 1
                 label("Log Cl")
                 tv <- 3.45
                 label("Log V")
                 add.err <- 0.7
                 eta.ka ~ 0.6
                 label("eta ka")
                 eta.cl ~ 0.3
                 label("eta cl")
                 eta.v ~ 0.1
                 label("eta v")
               })))

  expect_equal(lotriDataFrameToLotriExpression(df, useIni=TRUE),
               quote(ini({
                 tka <- 0.45
                 label("Log Ka")
                 tcl <- 1
                 label("Log Cl")
                 tv <- 3.45
                 label("Log V")
                 add.err <- 0.7
                 eta.ka ~ 0.6
                 label("eta ka")
                 eta.cl ~ 0.3
                 label("eta cl")
                 eta.v ~ 0.1
                 label("eta v")
               })))

  expect_equal(as.expression(x),
               quote(lotri({
                 tka <- 0.45
                 label("Log Ka")
                 tcl <- 1
                 label("Log Cl")
                 tv <- 3.45
                 label("Log V")
                 add.err <- 0.7
                 eta.ka ~ 0.6
                 label("eta ka")
                 eta.cl ~ 0.3
                 label("eta cl")
                 eta.v ~ 0.1
                 label("eta v")
               })))

  expect_equal(as.expression(x, useIni=TRUE),
               quote(ini({
                 tka <- 0.45
                 label("Log Ka")
                 tcl <- 1
                 label("Log Cl")
                 tv <- 3.45
                 label("Log V")
                 add.err <- 0.7
                 eta.ka ~ 0.6
                 label("eta ka")
                 eta.cl ~ 0.3
                 label("eta cl")
                 eta.v ~ 0.1
                 label("eta v")
               })))


  expect_error(lotriDataFrameToLotriExpression(rnorm))

})
