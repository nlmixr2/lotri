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

    expect_equal(tmp$df, list(id = 3, id2 = NULL))
    expect_equal(tmp$matt, NULL)

    expect_equal(tmp$id, structure(1, .Dim = c(1L, 1L),
                                   .Dimnames = list("et1", "et1")))

    expect_equal(tmp$.names, "df")

    expect_warning(utils::capture.output(print(tmp)), NA)

    expect_warning(utils::capture.output(str(tmp)), NA)

    expect_equal(.DollarNames(tmp, ""), c("id", "id2", ".names", ".list", "df"))

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

    ## lotri(et5 ~ 1,
    ##                    et2 + et3 ~ c(1,
    ##                                  2, 3),
    ##                    et1 ~ 3| id1 + id2)
    ## lotri(list(et2 + et3 + et4 ~ c(40,
    ##                                0.1, 20,
    ##                                0.1, 0.1, 30),
    ##            matrix(1,dimnames=list("et5","et5")) | id))

})
