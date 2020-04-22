context("lotri -- easy matrix parsing")
test_that("lotri matrix parsing", {

    expect_equal(lotri({et2 + et3 + et4 ~ c(40,
                                            0.1, 20,
                                            0.1, 0.1, 30)}),
                 structure(c(40, 0.1, 0.1, 0.1, 20, 0.1, 0.1, 0.1, 30),
                           .Dim = c(3L, 3L),
                           .Dimnames = list(c("et2", "et3", "et4"),
                                            c("et2", "et3", "et4"))))

    expect_equal(lotri(list(et2 + et3 + et4 ~ c(40,
                                                0.1, 20,
                                                0.1, 0.1, 30),
                            matrix(1,dimnames=list("et5","et5")))),
                 structure(c(40, 0.1, 0.1, 0, 0.1, 20, 0.1, 0, 0.1, 0.1, 30, 0,
                             0, 0, 0, 1),
                           .Dim = c(4L, 4L),
                           .Dimnames = list(c("et2", "et3", "et4", "et5"),
                                            c("et2", "et3", "et4", "et5"))))

    expect_equal(lotri(list(et2 + et3 + et4 ~ c(40,
                                                0.1, 20,
                                                0.1, 0.1, 30),
                            matrix(1,dimnames=list("et5","et5")))),
                 structure(c(40, 0.1, 0.1, 0, 0.1, 20, 0.1, 0, 0.1, 0.1, 30, 0,
                             0, 0, 0, 1),
                           .Dim = c(4L, 4L),
                           .Dimnames = list(c("et2", "et3", "et4", "et5"),
                                            c("et2", "et3", "et4", "et5"))))

    expect_equal(lotri({
        et2 + et3 + et4 ~ c(40,
                            0.1, 20,
                            0.1, 0.1, 30)
        et5 ~ 1}),
        structure(c(40, 0.1, 0.1, 0, 0.1, 20, 0.1, 0, 0.1, 0.1, 30, 0,
                    0, 0, 0, 1),
                  .Dim = c(4L, 4L),
                  .Dimnames = list(c("et2", "et3", "et4", "et5"),
                                   c("et2", "et3", "et4", "et5"))))
    expect_equal(lotri(et2 + et3 + et4 ~ c(40,
                                           0.1, 20,
                                           0.1, 0.1, 30),
                       et5 ~ 1),
                 structure(c(40, 0.1, 0.1, 0, 0.1, 20, 0.1, 0, 0.1, 0.1, 30, 0,
                             0, 0, 0, 1),
                           .Dim = c(4L, 4L),
                           .Dimnames = list(c("et2", "et3", "et4", "et5"),
                                            c("et2", "et3", "et4", "et5"))))

    expect_equal(lotri(et2 + et3 + et4 ~ c(40,
                                           0.1, 20,
                                           0.1, 0.1, 30),
                       list(et5 ~ 1, et6 ~ 3)),
                 structure(c(40, 0.1, 0.1, 0, 0, 0.1, 20, 0.1, 0,
                             0, 0.1, 0.1, 30, 0, 0, 0, 0, 0, 1, 0,
                             0, 0, 0, 0, 3),
                           .Dim = c(5L, 5L),
                           .Dimnames = list(c("et2", "et3", "et4",
                                              "et5", "et6"),
                                            c("et2", "et3", "et4",
                                              "et5", "et6"))))
    expect_equal(lotri(quote({et2 + et3 + et4 ~ c(40,
                                                  0.1, 20,
                                                  0.1, 0.1, 30)})),
                 structure(c(40, 0.1, 0.1, 0.1, 20, 0.1, 0.1, 0.1, 30),
                           .Dim = c(3L, 3L),
                           .Dimnames = list(c("et2", "et3", "et4"),
                                            c("et2", "et3", "et4"))))

    .mat  <- lotri({et2 + et3 + et4 ~ c(40,
                                        0.1, 20,
                                        0.1, 0.1, 30)})
    ## Test for NSE issues
    expect_equal(.mat, lotri(.mat))
    ## Test for NULL
    expect_equal(NULL, lotri(NULL))

    expect_equal(lotri(eta.Cl ~ 0.4^2),
                 structure(0.16, .Dim = c(1L, 1L),
                           .Dimnames = list("eta.Cl", "eta.Cl")))

    ## Parsing errors
    expect_error(lotri(a~fix(3)))
    expect_error(lotri(a~c(3,1,3)))
    expect_error(lotri(a~c(3,1)))
    expect_error(lotri({a <- c(3,1)}))

    expect_equal(lotri({matrix(3, dimnames=list("a","a"))}),
                 structure(3, .Dim = c(1L, 1L), .Dimnames = list("a", "a")))

    expect_equal(lotri(matrix(3, dimnames=list("a","a"))),
                 structure(3, .Dim = c(1L, 1L), .Dimnames = list("a", "a")))

    expect_error(lotri(quote(matrix(3, dimnames=list("a","a")))))

    expect_error(lotri(lotri(a ~ paste(1))))

    expect_equal(lotri({et2 + et3 + et4 ~ c(40,
                                            0.1, 20,
                                            0.1, 0.1, 30) | id}),
                 list(id = structure(c(40, 0.1, 0.1, 0.1, 20, 0.1,
                                       0.1, 0.1, 30), .Dim = c(3L, 3L),
                                     .Dimnames = list(c("et2", "et3", "et4"),
                                                      c("et2", "et3", "et4")
                                                      ))))

    expect_equal(lotri({et1 ~ c(40) | id}),
                 list(id = structure(c(40), .Dim = c(1L, 1L),
                                     .Dimnames = list(c("et1"),
                                                      c("et1")))))

    expect_equal(lotri({et1 ~ 40 | id}),
                 list(id = structure(c(40), .Dim = c(1L, 1L),
                                     .Dimnames = list(c("et1"),
                                                      c("et1")))))

    expect_equal(lotri({eta.Cl ~ 0.4^2 | id}),
                 list(id = structure(0.16, .Dim = c(1L, 1L),
                                     .Dimnames = list("eta.Cl", "eta.Cl"))))


    expect_equal(lotri(matrix(1,dimnames=list("et5","et5")) | id),
                 list(id = structure(1, .Dim = c(1L, 1L),
                                     .Dimnames = list("et5", "et5"))))

    expect_equal(lotri(matrix(1,dimnames=list("et5","et5")) | id,
                       matrix(1,dimnames=list("et1","et1")) | id),
                 list(id = structure(c(1, 0, 0, 1), .Dim = c(2L, 2L),
                                     .Dimnames = list(c("et5", "et1"),
                                                      c("et5", "et1")))))

    expect_equal(lotri(matrix(1,dimnames=list("et5","et5")) | id,
          matrix(1,dimnames=list("et2","et2")),
          matrix(1,dimnames=list("et1","et1")) | id),
          list(id = structure(c(1, 0, 0, 1),
                              .Dim = c(2L, 2L),
                              .Dimnames = list(c("et5", "et1"),
                                               c("et5", "et1"))),
               structure(1, .Dim = c(1L, 1L),
                         .Dimnames = list("et2", "et2"))))


    expect_equal(lotri(matrix(1,dimnames=list("et5","et5")) | id1,
          matrix(1,dimnames=list("et2","et2")) | id2,
          matrix(1,dimnames=list("et1","et1")) | id3),
          list(id1 = structure(1, .Dim = c(1L, 1L), .Dimnames = list("et5",
    "et5")), id2 = structure(1, .Dim = c(1L, 1L), .Dimnames = list(
    "et2", "et2")), id3 = structure(1, .Dim = c(1L, 1L), .Dimnames = list(
    "et1", "et1"))))


    expect_equal(lotri(et2 + et3 + et4 ~ c(40,
                              0.1, 20,
                              0.1, 0.1, 30),
                       list(et5 ~ 1, et6 ~ 3) | id),
                 list(structure(c(40, 0.1, 0.1, 0.1, 20, 0.1, 0.1, 0.1, 30),
                                .Dim = c(3L,3L),
                                .Dimnames = list(c("et2", "et3", "et4"),
                                                 c("et2", "et3","et4"))),
                      id = structure(c(1, 0, 0, 3),
                                     .Dim = c(2L, 2L),
                                     .Dimnames = list(c("et5", "et6"),
                                                      c("et5", "et6")))))

    expect_equal(lotri(list(et5 ~ 1, et6 ~ 3) | id),
                 list(id = structure(c(1, 0, 0, 3),
                                     .Dim = c(2L, 2L),
                                     .Dimnames = list(c("et5", "et6"),
                                                      c("et5", "et6")))))

    expect_equal(lotri(et5 ~ 1 | id1,
                       et2 + et3 ~ c(1,
                                     2, 3)| id2,
                       et1 ~ 3| id3),
                 list(id1 = structure(1, .Dim = c(1L, 1L),
                                      .Dimnames = list("et5", "et5")),
                      id2 = structure(c(1, 2, 2, 3),
                                      .Dim = c(2L, 2L),
                                      .Dimnames = list(c("et2", "et3"),
                                                       c("et2", "et3"))),
                      id3 = structure(3, .Dim = c(1L,1L),
                                      .Dimnames = list("et1", "et1"))))

    expect_equal(lotri(et5 ~ 1 | id1,
                       et2 + et3 ~ c(1,
                                     2, 3),
                       et1 ~ 3| id3),
                 list(id1 = structure(1, .Dim = c(1L, 1L),
                                      .Dimnames = list("et5", "et5")),
                      structure(c(1, 2, 2, 3),
                                .Dim = c(2L, 2L),
                                .Dimnames = list(c("et2", "et3"),
                                                 c("et2", "et3"))),
                      id3 = structure(3, .Dim = c(1L,1L),
                                      .Dimnames = list("et1", "et1"))))

    expect_equal(lotri(et5 ~ 1 | id1,
                       et2 + et3 ~ c(1,
                                     2, 3)| id2,
                       et1 ~ 3),
                 list(id1 = structure(1, .Dim = c(1L, 1L),
                                      .Dimnames = list("et5", "et5")),
                      id2 = structure(c(1, 2, 2, 3), .Dim = c(2L, 2L),
                                      .Dimnames = list(c("et2", "et3"),
                                                       c("et2", "et3"))),
                      structure(3, .Dim = c(1L, 1L),
                                .Dimnames = list("et1", "et1"))))


    expect_equal(lotri(et5 ~ 1 | id1,
                       et2 + et3 ~ c(1,
                                     2, 3),
                       et1 ~ 3| id1),
                 list(id1 = structure(c(1, 0, 0, 3),
                                      .Dim = c(2L, 2L),
                                      .Dimnames = list(c("et5", "et1"),
                                                       c("et5", "et1"))),
                      structure(c(1, 2, 2, 3),
                                .Dim = c(2L, 2L),
                                .Dimnames = list(c("et2", "et3"),
                                                 c("et2", "et3")))))

    expect_equal(lotri(et5 ~ 1,
                       et2 + et3 ~ c(1,
                                     2, 3),
                       et1 ~ 3| id1),
                 list(structure(c(1, 0, 0, 0, 1, 2, 0, 2, 3),
                                .Dim = c(3L, 3L),
                                .Dimnames = list(c("et5", "et2", "et3"),
                                                 c("et5", "et2", "et3"))),
                      id1 = structure(3, .Dim = c(1L,1L),
                                      .Dimnames = list("et1", "et1"))))


    expect_error(lotri(et1~c(1) | id + matt))

    tmp <- lotri(et1 ~ 1 | id(df=3), et2~3 | id2)

    expect_equal(tmp$df, list(id = 3))
    expect_equal(tmp$matt, NULL)

    expect_equal(tmp$id, structure(1, .Dim = c(1L, 1L),
                                   .Dimnames = list("et1", "et1")))

    expect_equal(tmp$.names, "df")

    expect_warning(utils::capture.output(print(tmp)), NA)

    expect_warning(utils::capture.output(str(tmp)), NA)

    expect_equal(.DollarNames(tmp, ""), c("id", "id2", ".names",
                                          ".list", "df"))

    expect_equal(.DollarNames(tmp, "i"), c("id", "id2", ".list"))

    expect_error(lotri(et1 ~ 1 | id(df=3), et2~3 | id(df=4)))

    tmp2 <- lotri(et1 ~ 1 | id(df=3), et2~3 | id(df2=4))

    expect_equal(tmp2$df, list(id=3))
    expect_equal(tmp2$df2, list(id=4))

    tmp2 <- lotri(et1 ~ 1 | id(lower=3))

    expect_equal(tmp2$lower, list(id = c(et1 = 3)))

    tmp2 <- lotri(et1 + et2 ~ c(1, 2, 3) | id(lower=3))

    expect_equal(tmp2$lower, list(id = c(et1 = 3, et2 = 3)))

    expect_error(lotri(et1 + et2 ~ c(1, 2, 3) | id(lower=c(2, 3))))

    expect_error(lotri(et1 + et2 ~ c(1, 2, 3) | id(lower=c(et3=4))))


    tmp2 <- lotri(et1 + et2 ~ c(1,
                                2, 3) | id(lower=3),
                  et3 ~ 3 | id(lower=4))

    expect_equal(tmp2$lower, list(id=c(et1=3, et2=3, et3=4)))

    tmp2 <- lotri(et1 + et2 ~ c(1,
                                2, 3) | id(lower=3),
                  et3 ~ 3 | id)

    expect_equal(tmp2$lower, list(id=c(et1=3, et2=3, et3= -Inf)))

    tmp2 <- lotri(et1 + et2 ~ c(1,
                                2, 3) | id(upper=3),
                  et3 ~ 3 | id)

    expect_equal(tmp2$upper, list(id=c(et1=3, et2=3, et3= Inf)))

    expect_equal(tmp2$lower, list(id = c(et1 = -Inf, et2 = -Inf, et3 = -Inf)))

    tmp2 <- lotri(et1 + et2 ~ c(1,
                                2, 3) | id(lower=c(et2=3)),
                  et3 ~ 3 | id)

    expect_equal(tmp2$lower,
                 list(id = c(et1 = -Inf, et2 = 3, et3 = -Inf)))

    tmp2 <- lotri(et1 + et2 ~ c(1,
                                2, 3) | id(upper=c(et2=3)),
                  et3 ~ 3 | id)

    expect_equal(tmp2$upper,
                 list(id = c(et1 = Inf, et2 = 3, et3 = Inf)))


    tmp2 <- lotri(eta.Cl ~ 0.1,
                  eta.Ka ~ 0.2,
                  inv.Cl ~ 0.3,
                  inv.Ka ~ 0.4,
                  iov.Ka ~ 0.5,
                  iov.Cl ~ 0.6 | occ(lower=3))

    expect_equal(tmp2,
                 structure(list(structure(c(0.1, 0, 0, 0, 0, 0, 0.2, 0,
                                            0, 0, 0, 0, 0.3, 0, 0, 0, 0,
                                            0, 0.4, 0, 0, 0, 0, 0, 0.5),
                                          .Dim = c(5L, 5L),
                                          .Dimnames = list(c("eta.Cl",
                                                             "eta.Ka",
                                                             "inv.Cl",
                                                             "inv.Ka",
                                                             "iov.Ka"),
                                                           c("eta.Cl",
                                                             "eta.Ka",
                                                             "inv.Cl",
                                                             "inv.Ka",
                                                             "iov.Ka"))),
                                occ = structure(0.6, .Dim = c(1L, 1L),
                                                .Dimnames = list("iov.Cl",
                                                                 "iov.Cl"))),
                           lotri = list(occ = list(lower = c(iov.Cl = 3))), class = "lotri"))

    tmp2 <- lotri(inv.Ka ~ 0.4,
          iov.Ka ~ 0.5 | occ,
          iov.Cl ~ 0.6 | occ(lower=3)
          )

    expect_equal(tmp2$lower$occ, c(iov.Ka = -Inf, iov.Cl = 3))


    tmp2 <- lotri(inv.Ka ~ 0.4,
          iov.Ka ~ 0.5 | occ(lower=3),
          iov.Cl ~ 0.6 | occ
          )

    expect_equal(tmp2$lower$occ, c(iov.Ka = 3, iov.Cl = -Inf))

    tmp2 <- lotri(eta.Cl ~ 0.1,
          eta.Ka ~ 0.2,
          inv.Cl ~ 0.3,
          inv.Ka ~ 0.4 | occ,
          iov.Ka ~ 0.5,
          iov.Cl ~ 0.6 | occ(lower=3)
          )

    expect_equal(tmp2$lower$occ, c(inv.Ka = -Inf, iov.Cl = 3))

    tmp2 <- lotri(eta.Cl ~ 0.1,
          eta.Ka ~ 0.2,
          inv.Cl ~ 0.3,
          inv.Ka ~ 0.4 | occ(lower=3),
          iov.Ka ~ 0.5,
          iov.Cl ~ 0.6 | occ
          )

    expect_equal(tmp2$lower$occ, c(inv.Ka = 3, iov.Cl = -Inf))

    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                        iov.Cl ~ 0.6) | occ(lower=3))

    expect_equal(tmp2$lower, list(occ = c(iov.Ka = 3, iov.Cl = 3)))

    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                         iov.Cl ~ 0.6) | iov(lower=3),
                  lotri(occ.Ka ~ 0.5,
                        occ.Cl ~ 0.6) | occ(lower=4))

    expect_equal(tmp2$lower,
                 list(iov = c(iov.Ka = 3, iov.Cl = 3),
                      occ = c(occ.Ka = 4, occ.Cl = 4)))

    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                         iov.Cl ~ 0.6) | iov(lower=3),
                  lotri(occ.Ka ~ 0.5,
                        occ.Cl ~ 0.6) | iov(lower=4))

    expect_equal(tmp2$lower,
                 list(iov = c(iov.Ka = 3, iov.Cl = 3,
                              occ.Ka = 4, occ.Cl = 4)))


    tmp2 <- lotri(
        lotri(eta.Cl ~ 0.1,
              eta.Ka ~ 0.2),
        lotri(inv.Ka ~ 0.3,
              inv.Cl ~ 0.4) | inv(lower=2),
        lotri(iov.Ka ~ 0.5,
              iov.Cl ~ 0.6) | occ(lower=3))

    expect_equal(tmp2$lower,
                 list(c(eta.Cl = -Inf, eta.Ka = -Inf),
                      inv = c(inv.Ka = 2, inv.Cl = 2),
                      occ = c(iov.Ka = 3, iov.Cl = 3)))

    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                        iov.Cl ~ 0.6) | occ)

    expect_equal(tmp2,
                 list(occ = structure(c(0.5, 0, 0, 0.6),
                                      .Dim = c(2L, 2L),
                                      .Dimnames = list(c("iov.Ka",
                                                         "iov.Cl"),
                                                       c("iov.Ka",
                                                         "iov.Cl")))))

    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                         iov.Cl ~ 0.6) | iov,
                  lotri(occ.Ka ~ 0.5,
                        occ.Cl ~ 0.6) | occ(lower=4))

    expect_equal(tmp2,
                 structure(list(iov = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L,
2L), .Dimnames = list(c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl"
))), occ = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
    c("occ.Ka", "occ.Cl"), c("occ.Ka", "occ.Cl")))), lotri = list(
    occ = list(lower = c(occ.Ka = 4, occ.Cl = 4))), class = "lotri"))


    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                         iov.Cl ~ 0.6) | iov(lower=3),
                  lotri(occ.Ka ~ 0.5,
                        occ.Cl ~ 0.6) | occ)

    expect_equal(tmp2,
                 structure(list(iov = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L,
2L), .Dimnames = list(c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl"
))), occ = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
    c("occ.Ka", "occ.Cl"), c("occ.Ka", "occ.Cl")))), lotri = list(
    iov = list(lower = c(iov.Ka = 3, iov.Cl = 3))), class = "lotri"))

    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                         iov.Cl ~ 0.6) | iov,
                  lotri(occ.Ka ~ 0.5,
                        occ.Cl ~ 0.6) | occ)

    expect_equal(tmp2,
                 list(iov = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
    c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl"))), occ = structure(c(0.5,
0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(c("occ.Ka", "occ.Cl"
), c("occ.Ka", "occ.Cl")))))


    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                         iov.Cl ~ 0.6),
                  lotri(occ.Ka ~ 0.5,
                        occ.Cl ~ 0.6) | occ(lower=4))

    expect_equal(tmp2, structure(list(structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
    c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl"))), occ = structure(c(0.5,
0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(c("occ.Ka", "occ.Cl"
), c("occ.Ka", "occ.Cl")))), lotri = list(occ = list(lower = c(occ.Ka = 4,
occ.Cl = 4))), class = "lotri"))

    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                         iov.Cl ~ 0.6) | iov(lower=3),
                  lotri(occ.Ka ~ 0.5,
                        occ.Cl ~ 0.6))

    expect_equal(tmp2,
                 structure(list(iov = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L,
2L), .Dimnames = list(c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl"
))), 0.5, 0, 0, 0.6), lotri = list(iov = list(lower = c(iov.Ka = 3,
iov.Cl = 3))), class = "lotri"))

    context("as.lotri")
    tmp2 <- lotri(iov.Ka ~ 0.5,
                  iov.Cl ~ 0.6)

    tmp3 <- as.lotri(tmp2)

    expect_equal(tmp3, structure(list(structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
    c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl")))), .Names = "", class = "lotri"))

    tmp3 <- as.lotri(tmp3, default="id")

    expect_equal(tmp3, structure(list(id = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L,
                                                                                 2L), .Dimnames = list(c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl")))), class = "lotri"))

    expect_true(inherits(as.matrix(tmp3), "matrix"))

    tmp2 <- lotri(lotri(iov.Ka ~ 0.5,
                         iov.Cl ~ 0.6) | iov(lower=3),
                  lotri(occ.Ka ~ 0.5,
                        occ.Cl ~ 0.6))

    expect_error(as.matrix(tmp2))

    l1 <- as.lotri(lotri(et1+et2 ~c(0.1, 0.01, 1)), lower=4, default="id")
    
    l2 <- lotri(et1+et2 ~c(0.1, 0.01, 1) | id(lower=4))

    expect_equal(l1,l2)

    l1 <- as.lotri(lotri(et1+et2 ~c(0.1, 0.01, 1)), nu=4, default="id")
    l2 <- lotri(et1+et2 ~c(0.1, 0.01, 1) | id(nu=4))

    expect_equal(l1,l2)

    l1 <- as.lotri(lotri(et1+et2 ~c(0.1, 0.01, 1)), upper=c(et1=3), default="id")
    l2 <- lotri(et1+et2 ~c(0.1, 0.01, 1) | id(upper=c(et1=3)))

    expect_equal(l1,l2)


    l1 <- as.lotri(lotri(et1+et2 ~c(0.1, 0.01, 1)), upper=c(et1=3), matt=NULL, default="id")
    l2 <- lotri(et1+et2 ~c(0.1, 0.01, 1) | id(upper=c(et1=3)))

    expect_equal(l1, l2)

    expect_error(as.lotri(lotri(et1+et2 ~c(0.1, 0.01, 1)), upper=c(3,3), default="id"))
    expect_error(as.lotri(lotri(et1+et2 ~c(0.1, 0.01, 1)), upper=1L, default="id"))

    context("lotriMat");

    expect_error(.Call(lotri:::`_asLotriMat`, "a", list(nu=3), "id", PACKAGE="lotri"))

    expect_error(.Call(lotri:::`_asLotriMat`, matrix(1), list(nu=3), "id", PACKAGE="lotri"))
    expect_error(.Call(lotri:::`_asLotriMat`, structure(1, .Dim = c(1L, 1L), dimnames=list(NULL, c("a"))),
                       list(nu=3), "id", PACKAGE="lotri"))
    expect_error(.Call(lotri:::`_asLotriMat`, structure(1, .Dim = c(1L, 1L), dimnames=list(c("a"), NULL)),
                       list(nu=3), "id", PACKAGE="lotri"))

    expect_error(.Call(lotri:::`_asLotriMat`, lotri(et1+et2 ~c(0.1, 0.01, 1)),
                       "a", "id", PACKAGE="lotri"))
    
    expect_error(as.lotri(lotri(et1+et2 ~c(0.1, 0.01, 1)), upper=1L, default=c("id","id2")))
    expect_error(as.lotri(lotri(et1+et2 ~c(0.1, 0.01, 1)), upper=1L, default=3))

    expect_error(as.lotri(lotri(et1+et2 ~c(0.1, 0.01, 1)), lower=4))

    testList <- list(lotri({et2 + et3 + et4 ~ c(40,
                            0.1, 20,
                            0.1, 0.1, 30)}),
                     lotri(et5 ~ 6),
                     lotri(et1+et6 ~c(0.1, 0.01, 1)),
                     matrix(c(1L, 0L, 0L, 1L), 2, 2,
                            dimnames=list(c("et7", "et8"),
                                          c("et7", "et8"))))

    expect_equal(lotriMat(testList),
                 structure(c(40, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 20, 0.1, 0, 0, 
0, 0, 0, 0.1, 0.1, 30, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 
0, 0, 0, 0, 0.1, 0.01, 0, 0, 0, 0, 0, 0, 0.01, 1, 0, 0, 0, 0, 
0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1), .Dim = c(8L, 8L), .Dimnames = list(
    c("et2", "et3", "et4", "et5", "et1", "et6", "et7", "et8"), 
    c("et2", "et3", "et4", "et5", "et1", "et6", "et7", "et8"))))

    expect_error(lotriMat(list(lotri({et2 + et3 + et4 ~ c(40,
                            0.1, 20,
                            0.1, 0.1, 30)}),
                            "A")))

    expect_error(lotriMat(3))

    testList <- list(lotri({et2 + et3 + et4 ~ c(40,
                            0.1, 20,
                            0.1, 0.1, 30)}),
                     list(lotri(et5 ~ 6), 3),
                     lotri(et1+et6 ~c(0.1, 0.01, 1)),
                     matrix(c(1L, 0L, 0L, 1L), 2, 2,
                            dimnames=list(c("et7", "et8"),
                                          c("et7", "et8"))))

    testList1 <- list(lotri({et2 + et3 + et4 ~ c(40,
                            0.1, 20,
                            0.1, 0.1, 30)}),
                     list(lotri(et5 ~ 6), 3L),
                     lotri(et1+et6 ~c(0.1, 0.01, 1)),
                     matrix(c(1L, 0L, 0L, 1L), 2, 2,
                            dimnames=list(c("et7", "et8"),
                                          c("et7", "et8"))))

    expect_equal(lotriMat(testList), lotriMat(testList1))

    expect_error(lotriMat(testList,4))
    expect_error(lotriMat(testList,"eta[%d]", "a"))

    expect_equal(dimnames(lotriMat(testList,"ETA[%d]", start=3))[[1]],
                 c("et2", "et3", "et4", "ETA[3]", "ETA[4]", "ETA[5]",
                   "et1", "et6", "et7", "et8"))

    expect_equal(dimnames(lotriMat(testList,"ETA[%d]"))[[1]],
                 c("et2", "et3", "et4", "ETA[1]", "ETA[2]", "ETA[3]",
                   "et1", "et6", "et7", "et8"))

    testList <- list(lotri({et2 + et3 + et4 ~ c(40,
                            0.1, 20,
                            0.1, 0.1, 30)}),
                     list(lotri(et5 ~ 6), 3, 4),
                     lotri(et1+et6 ~c(0.1, 0.01, 1)),
                     matrix(c(1L, 0L, 0L, 1L), 2, 2,
                            dimnames=list(c("et7", "et8"),
                                          c("et7", "et8"))))
    expect_error(lotriMat(testList))

    testList <- list(lotri({et2 + et3 + et4 ~ c(40,
                            0.1, 20,
                            0.1, 0.1, 30)}),
                     list(lotri(et5 ~ 6), 0),
                     lotri(et1+et6 ~c(0.1, 0.01, 1)),
                     matrix(c(1L, 0L, 0L, 1L), 2, 2,
                            dimnames=list(c("et7", "et8"),
                                          c("et7", "et8"))))
    expect_error(lotriMat(testList))

    testList <- list(lotri({et2 + et3 + et4 ~ c(40,
                            0.1, 20,
                            0.1, 0.1, 30)}),
                     list(lotri(et5 ~ 6), 1:3),
                     lotri(et1+et6 ~c(0.1, 0.01, 1)),
                     matrix(c(1L, 0L, 0L, 1L), 2, 2,
                            dimnames=list(c("et7", "et8"),
                                          c("et7", "et8"))))

    expect_error(lotriMat(testList))

    mat1 <- lotri({et2 + et3 + et4 ~ c(40,
                            0.1, 20,
                            0.1, 0.1, 30)})
    
    expect_equal(mat1, lotriMat(mat1))

    mat1 <- list(mat1, 3)

    expect_equal(lotriMat(mat1),lotriMat(list(mat1)))
    expect_equal(lotriMat(mat1, "ETA[%d]"),lotriMat(list(mat1),"ETA[%d]"))
    expect_equal(lotriMat(mat1, "ETA[%d]",4),lotriMat(list(mat1),"ETA[%d]",4L))

    context("lotriSep");

    omega <- lotri(lotri(eta.Cl ~ 0.1,
                             eta.Ka ~ 0.1) | id(nu=100),
                       lotri(eye.Cl ~ 0.05,
                             eye.Ka ~ 0.05) | eye(nu=50),
                       lotri(iov.Cl ~ 0.01,
                             iov.Ka ~ 0.01) | occ(nu=200),
                       lotri(inv.Cl ~ 0.02,
                             inv.Ka ~ 0.02) | inv(nu=10))

    sepA <- lotriSep(omega, above=c(inv=10L), below=c(eye=2L, occ=4L))

    sepB <- list(above=lotri(lotri(inv.Cl ~ 0.02,
                                    inv.Ka ~ 0.02) |
                              inv(nu=100, same=10L)),
                  below=lotri(lotri(eta.Cl ~ 0.1,
                                    eta.Ka ~ 0.1) | id(nu=100),
                              lotri(eye.Cl ~ 0.05,
                                    eye.Ka ~ 0.05) | eye(nu=50, same=2L),
                              lotri(iov.Cl ~ 0.01,
                                    iov.Ka ~ 0.01) | occ(nu=200, same=4L)))

    attr(sepB$below, "format") <- "ETA[%d]"
    attr(sepB$below, "start")  <- 1L

    attr(sepB$above, "format") <- "THETA[%d]"
    attr(sepB$above, "start")  <- 1L

    expect_equal(sepA, sepB)

    expect_equal(dimnames(lotriMat(sepA$above))[[1]],
                 sprintf("THETA[%d]", 1:20))
    expect_equal(dimnames(lotriMat(sepA$below))[[1]],
                 c("eta.Cl","eta.Ka",sprintf("ETA[%d]", 1:12)))

    above1 <- attr(sepA$above, "lotri")
    above1$inv$same <- "matt"
    above <- sepA$above;
    attr(above, "lotri") <- above1

    expect_equal(dimnames(lotriMat(above))[[1]],c("inv.Cl", "inv.Ka"))

    expect_error(lotriSep(omega, above=c(inv=10L), below=c(eye=2L, occ=4L), aboveStart=1:2))

    expect_error(lotriSep(omega, above=c(inv=10), below=c(eye=2L, occ=4L)))

    expect_error(lotriSep(omega, above=c(inv=10L), below=c(eye=2, occ=4)))

    expect_error(lotriSep(omega, above=c(10L), below=c(eye=2L, occ=4L)))

    expect_error(lotriSep(omega, above=c(inv=10L), below=c(2L, 4L)))

    expect_error(lotriSep(omega, above=c(inv=10L), below=c(eye=2L, matt=4L), aboveStart=2L))

    omega0 <- lotri(lotri(eye.Cl ~ 0.05,
                          eye.Ka ~ 0.05) | eye(nu=50),
                    lotri(iov.Cl ~ 0.01,
                          iov.Ka ~ 0.01) | occ(nu=200),
                    lotri(inv.Cl ~ 0.02,
                          inv.Ka ~ 0.02) | inv(nu=10))

    expect_error(lotriSep(omega0, above=c(inv=10L), below=c(eye=2L, occ=4L), aboveStart=2L))

    sepA <- lotriSep(omega, above=c(), below=c(eye=2L, occ=4L))
    
    expect_equal(sepA$above, NULL)

    sepA <- lotriSep(omega, above=NULL, below=c(eye=2L, occ=4L))

    expect_equal(sepA$above, NULL)

    ## Bad Lotri matrix

    omega1 <- structure(list(id = structure(c(0.1, 0, 0, 0.1), .Dim = c(2L, 
2L), .Dimnames = list(c("eta.Cl", "eta.Ka"), c("eta.Cl", "eta.Ka"
))), eye = structure(c(0.05, 0, 0, 0.05), .Dim = c(2L, 2L), .Dimnames = list(
    c("eye.Cl", "eye.Ka"), c("eye.Cl", "eye.Ka"))), occ = structure(c(0.01, 
0, 0, 0.01), .Dim = c(2L, 2L), .Dimnames = list(c("iov.Cl", "iov.Ka"
), c("iov.Cl", "iov.Ka"))), inv = structure(c(0.02, 0, 0, 0.02
), .Dim = c(2L, 2L), .Dimnames = list(c("inv.Cl", "inv.Ka"), 
    c("inv.Cl", "inv.Ka")))), lotri = list(id = list(nu = 100), 
    eye = list(nu = 50), inv = list(nu = 10)), class = "lotri")
    
    expect_error(lotriSep(omega1, above=c(), below=c(eye=2L, occ=4L)))

    context("allNames")

    omega <- lotri(lotri(eta.Cl ~ 0.1,
                         eta.Ka ~ 0.1) | id(nu=100),
                   lotri(eye.Cl ~ 0.05,
                         eye.Ka ~ 0.05) | eye(nu=50),
                   lotri(iov.Cl ~ 0.01,
                         iov.Ka ~ 0.01) | occ(nu=200),
                   lotri(inv.Cl ~ 0.02,
                         inv.Ka ~ 0.02) | inv(nu=10))

    expect_equal(omega$.allNames,
                 c("inv.Cl", "inv.Ka", "iov.Cl", "iov.Ka",
                   "eye.Cl", "eye.Ka", "eta.Cl", "eta.Ka"))

    expect_error(.Call(lotri:::`_lotriAllNames`, 1:20, PACKAGE="lotri"))

    
    mat0 <- lotri(eta.Cl ~ 0.1,
                  eta.Ka ~ 0.1)
    dn <- dimnames(mat0)
    dn0 <- list(dn[[1]],NULL)
    dimnames(mat0) <- dn0
    
    dn1 <- .Call(lotri:::`_lotriAllNames`, mat0, PACKAGE="lotri")

    dn0 <- list(NULL, dn[[1]])
    dimnames(mat0) <- dn0
    
    dn2 <- .Call(lotri:::`_lotriAllNames`, mat0, PACKAGE="lotri")

    expect_equal(dn1, dn2)

    dn0 <- list(NULL, NULL)
    dimnames(mat0) <- dn0

    dn3 <- .Call(lotri:::`_lotriAllNames`, mat0, PACKAGE="lotri")

    expect_equal(dn3, character(0))

    dimnames(mat0) <- NULL

    dn3 <- .Call(lotri:::`_lotriAllNames`, mat0, PACKAGE="lotri")

    expect_equal(dn3, character(0))

})
