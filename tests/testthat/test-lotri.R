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


test_that("lotri matrix parsing", {


  expect_equal(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    structure(c(40, 0.1, 0.1, 0.1, 20, 0.1, 0.1, 0.1, 30),
      .Dim = c(3L, 3L),
      .Dimnames = list(
        c("et2", "et3", "et4"),
        c("et2", "et3", "et4")
      )
    )
  )

  expect_equal(
    lotri(list(
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      ),
      matrix(1, dimnames = list("et5", "et5"))
    )),
    structure(c(
      40, 0.1, 0.1, 0, 0.1, 20, 0.1, 0, 0.1, 0.1, 30, 0,
      0, 0, 0, 1
    ),
    .Dim = c(4L, 4L),
    .Dimnames = list(
      c("et2", "et3", "et4", "et5"),
      c("et2", "et3", "et4", "et5")
    )
    )
  )

  expect_equal(
    lotri(list(
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      ),
      matrix(1, dimnames = list("et5", "et5"))
    )),
    structure(c(
      40, 0.1, 0.1, 0, 0.1, 20, 0.1, 0, 0.1, 0.1, 30, 0,
      0, 0, 0, 1
    ),
    .Dim = c(4L, 4L),
    .Dimnames = list(
      c("et2", "et3", "et4", "et5"),
      c("et2", "et3", "et4", "et5")
    )
    )
  )

  expect_equal(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
      et5 ~ 1
    }),
    structure(c(
      40, 0.1, 0.1, 0, 0.1, 20, 0.1, 0, 0.1, 0.1, 30, 0,
      0, 0, 0, 1
    ),
    .Dim = c(4L, 4L),
    .Dimnames = list(
      c("et2", "et3", "et4", "et5"),
      c("et2", "et3", "et4", "et5")
    )
    )
  )
  expect_equal(
    lotri(
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      ),
      et5 ~ 1
    ),
    structure(c(
      40, 0.1, 0.1, 0, 0.1, 20, 0.1, 0, 0.1, 0.1, 30, 0,
      0, 0, 0, 1
    ),
    .Dim = c(4L, 4L),
    .Dimnames = list(
      c("et2", "et3", "et4", "et5"),
      c("et2", "et3", "et4", "et5")
    )
    )
  )

  expect_equal(
    lotri(
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      ),
      list(et5 ~ 1, et6 ~ 3)
    ),
    structure(c(
      40, 0.1, 0.1, 0, 0, 0.1, 20, 0.1, 0,
      0, 0.1, 0.1, 30, 0, 0, 0, 0, 0, 1, 0,
      0, 0, 0, 0, 3
    ),
    .Dim = c(5L, 5L),
    .Dimnames = list(
      c(
        "et2", "et3", "et4",
        "et5", "et6"
      ),
      c(
        "et2", "et3", "et4",
        "et5", "et6"
      )
    )
    )
  )
  expect_equal(
    lotri(quote({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    })),
    structure(c(40, 0.1, 0.1, 0.1, 20, 0.1, 0.1, 0.1, 30),
      .Dim = c(3L, 3L),
      .Dimnames = list(
        c("et2", "et3", "et4"),
        c("et2", "et3", "et4")
      )
    )
  )

  .mat <- lotri({
    et2 + et3 + et4 ~ c(
      40,
      0.1, 20,
      0.1, 0.1, 30
    )
  })
  ## Test for NSE issues
  expect_equal(.mat, lotri(.mat))
  ## Test for NULL
  expect_equal(NULL, lotri(NULL))

  expect_equal(
    lotri(eta.Cl ~ 0.4^2),
    structure(0.16,
      .Dim = c(1L, 1L),
      .Dimnames = list("eta.Cl", "eta.Cl")
    )
  )

  ## Parsing errors
  expect_error(lotri(~ c(40))) #nolint
  expect_error(lotri(~ 40))
  expect_error(lotri(a ~ c(3, 1, 3)))
  expect_error(lotri(a ~ c(3, 1)))
  ## expect_error(lotri({
  ##   a <- c(3, 1)
  ## }))

  expect_equal(
    lotri({
      matrix(3, dimnames = list("a", "a"))
    }),
    structure(3, .Dim = c(1L, 1L), .Dimnames = list("a", "a"))
  )

  expect_error(lotri({
    matrix(3, dimnames = list("a", "a"))
    matrix(3, dimnames = list("b", "b"))
  }))

  expect_equal(
    lotri(matrix(3, dimnames = list("a", "a"))),
    structure(3, .Dim = c(1L, 1L), .Dimnames = list("a", "a"))
  )

  expect_error(lotri(quote(matrix(3, dimnames = list("a", "a")))))

  expect_error(lotri(lotri(a ~ paste(1))))

  expect_equal(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      ) | id
    }),
    list(id = structure(c(
      40, 0.1, 0.1, 0.1, 20, 0.1,
      0.1, 0.1, 30
    ),
    .Dim = c(3L, 3L),
    .Dimnames = list(
      c("et2", "et3", "et4"),
      c("et2", "et3", "et4")
    )
    ))
  )

  expect_equal(
    lotri({
      et1 ~ c(40) | id }), # nolint
    list(id = structure(40,
      .Dim = c(1L, 1L),
      .Dimnames = list(
        "et1",
        "et1"
      )
    ))
  )

  expect_equal(
    lotri({
      et1 ~ 40 | id
    }),
    list(id = structure(40,
      .Dim = c(1L, 1L),
      .Dimnames = list(
        "et1",
        "et1"
      )
    ))
  )

  expect_equal(
    lotri({
      eta.Cl ~ 0.4^2 | id
    }),
    list(id = structure(0.16,
      .Dim = c(1L, 1L),
      .Dimnames = list("eta.Cl", "eta.Cl")
    ))
  )


  expect_equal(
    lotri(matrix(1, dimnames = list("et5", "et5")) | id),
    list(id = structure(1,
      .Dim = c(1L, 1L),
      .Dimnames = list("et5", "et5")
    ))
  )

  expect_equal(
    lotri(
      matrix(1, dimnames = list("et5", "et5")) | id,
      matrix(1, dimnames = list("et1", "et1")) | id
    ),
    list(id = structure(c(1, 0, 0, 1),
      .Dim = c(2L, 2L),
      .Dimnames = list(
        c("et5", "et1"),
        c("et5", "et1")
      )
    ))
  )

  expect_equal(
    lotri(
      matrix(1, dimnames = list("et5", "et5")) | id,
      matrix(1, dimnames = list("et2", "et2")),
      matrix(1, dimnames = list("et1", "et1")) | id
    ),
    list(
      id = structure(c(1, 0, 0, 1),
        .Dim = c(2L, 2L),
        .Dimnames = list(
          c("et5", "et1"),
          c("et5", "et1")
        )
      ),
      structure(1,
        .Dim = c(1L, 1L),
        .Dimnames = list("et2", "et2")
      )
    )
  )


  expect_equal(
    lotri(
      matrix(1, dimnames = list("et5", "et5")) | id1,
      matrix(1, dimnames = list("et2", "et2")) | id2,
      matrix(1, dimnames = list("et1", "et1")) | id3
    ),
    list(id1 = structure(1, .Dim = c(1L, 1L), .Dimnames = list(
      "et5",
      "et5"
    )), id2 = structure(1, .Dim = c(1L, 1L), .Dimnames = list(
      "et2", "et2"
    )), id3 = structure(1, .Dim = c(1L, 1L), .Dimnames = list(
      "et1", "et1"
    )))
  )


  expect_equal(
    lotri(
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      ),
      list(et5 ~ 1, et6 ~ 3) | id
    ),
    list(structure(c(40, 0.1, 0.1, 0.1, 20, 0.1, 0.1, 0.1, 30),
      .Dim = c(3L, 3L),
      .Dimnames = list(
        c("et2", "et3", "et4"),
        c("et2", "et3", "et4")
      )
    ),
    id = structure(c(1, 0, 0, 3),
      .Dim = c(2L, 2L),
      .Dimnames = list(
        c("et5", "et6"),
        c("et5", "et6")
      )
    )
    )
  )

  expect_equal(
    lotri(list(et5 ~ 1, et6 ~ 3) | id),
    list(id = structure(c(1, 0, 0, 3),
      .Dim = c(2L, 2L),
      .Dimnames = list(
        c("et5", "et6"),
        c("et5", "et6")
      )
    ))
  )

  expect_equal(
    lotri(
      et5 ~ 1 | id1,
      et2 + et3 ~ c(
        1,
        2, 3
      ) | id2,
      et1 ~ 3 | id3
    ),
    list(
      id1 = structure(1,
        .Dim = c(1L, 1L),
        .Dimnames = list("et5", "et5")
      ),
      id2 = structure(c(1, 2, 2, 3),
        .Dim = c(2L, 2L),
        .Dimnames = list(
          c("et2", "et3"),
          c("et2", "et3")
        )
      ),
      id3 = structure(3,
        .Dim = c(1L, 1L),
        .Dimnames = list("et1", "et1")
      )
    )
  )

  expect_equal(
    lotri(
      et5 ~ 1 | id1,
      et2 + et3 ~ c(
        1,
        2, 3
      ),
      et1 ~ 3 | id3
    ),
    list(
      id1 = structure(1,
        .Dim = c(1L, 1L),
        .Dimnames = list("et5", "et5")
      ),
      structure(c(1, 2, 2, 3),
        .Dim = c(2L, 2L),
        .Dimnames = list(
          c("et2", "et3"),
          c("et2", "et3")
        )
      ),
      id3 = structure(3,
        .Dim = c(1L, 1L),
        .Dimnames = list("et1", "et1")
      )
    )
  )

  expect_equal(
    lotri(
      et5 ~ 1 | id1,
      et2 + et3 ~ c(
        1,
        2, 3
      ) | id2,
      et1 ~ 3
    ),
    list(
      id1 = structure(1,
        .Dim = c(1L, 1L),
        .Dimnames = list("et5", "et5")
      ),
      id2 = structure(c(1, 2, 2, 3),
        .Dim = c(2L, 2L),
        .Dimnames = list(
          c("et2", "et3"),
          c("et2", "et3")
        )
      ),
      structure(3,
        .Dim = c(1L, 1L),
        .Dimnames = list("et1", "et1")
      )
    )
  )


  expect_equal(
    lotri(
      et5 ~ 1 | id1,
      et2 + et3 ~ c(
        1,
        2, 3
      ),
      et1 ~ 3 | id1
    ),
    list(
      id1 = structure(c(1, 0, 0, 3),
        .Dim = c(2L, 2L),
        .Dimnames = list(
          c("et5", "et1"),
          c("et5", "et1")
        )
      ),
      structure(c(1, 2, 2, 3),
        .Dim = c(2L, 2L),
        .Dimnames = list(
          c("et2", "et3"),
          c("et2", "et3")
        )
      )
    )
  )

  expect_equal(
    lotri(
      et5 ~ 1,
      et2 + et3 ~ c(
        1,
        2, 3
      ),
      et1 ~ 3 | id1
    ),
    list(structure(c(1, 0, 0, 0, 1, 2, 0, 2, 3),
      .Dim = c(3L, 3L),
      .Dimnames = list(
        c("et5", "et2", "et3"),
        c("et5", "et2", "et3")
      )
    ),
    id1 = structure(3,
      .Dim = c(1L, 1L),
      .Dimnames = list("et1", "et1")
    )
    )
  )


  expect_error(lotri(et1 ~ c(1) | id + matt)) # nolint
  expect_error(lotri(et1 ~ 1 | id + matt))

  tmp <- lotri(et1 ~ 1 | id(df = 3), et2 ~ 3 | id2)

  expect_equal(tmp$df, list(id = 3))
  expect_equal(tmp$matt, NULL)

  expect_equal(tmp$id, structure(1,
    .Dim = c(1L, 1L),
    .Dimnames = list("et1", "et1")
  ))

  expect_equal(tmp$.names, "df")

  expect_snapshot_output(print(tmp))

  expect_snapshot_output(str(tmp))

  expect_equal(.DollarNames(tmp, ""), c("id", "id2", ".allNames", ".bounds", ".names", ".list", ".maxNu", "df"))

  expect_equal(.DollarNames(tmp, "i"), c("id", "id2", ".list"))

  expect_error(lotri(et1 ~ 1 | id(df = 3), et2 ~ 3 | id(df = 4)))

  tmp2 <- lotri(et1 ~ 1 | id(df = 3), et2 ~ 3 | id(df2 = 4))

  expect_equal(tmp2$df, list(id = 3))
  expect_equal(tmp2$df2, list(id = 4))

  tmp2 <- lotri(et1 ~ 1 | id(lower = 3))

  expect_equal(tmp2$lower, list(id = c(et1 = 3)))

  tmp2 <- lotri(et1 + et2 ~ c(1, 2, 3) | id(lower = 3))

  expect_equal(tmp2$lower, list(id = c(et1 = 3, et2 = 3)))

  expect_error(lotri(et1 + et2 ~ c(1, 2, 3) | id(lower = c(2, 3))))

  expect_error(lotri(et1 + et2 ~ c(1, 2, 3) | id(lower = c(et3 = 4))))

  expect_error(lotri(et1 + et2 ~ c(1, 2, 3) | id(upper = c(2, 3))))

  expect_error(lotri(et1 + et2 ~ c(1, 2, 3) | id(upper = c(et3 = 4))))


  tmp2 <- lotri(
    et1 + et2 ~ c(
      1,
      2, 3
    ) | id(lower = 3),
    et3 ~ 3 | id(lower = 4)
  )

  expect_equal(tmp2$lower, list(id = c(et1 = 3, et2 = 3, et3 = 4)))

  tmp2 <- lotri(
    et1 + et2 ~ c(
      1,
      2, 3
    ) | id(lower = 3),
    et3 ~ 3 | id
  )

  expect_equal(tmp2$lower, list(id = c(et1 = 3, et2 = 3, et3 = -Inf)))

  tmp2 <- lotri(
    et1 + et2 ~ c(
      1,
      2, 3
    ) | id(upper = 3),
    et3 ~ 3 | id
  )

  expect_equal(tmp2$upper, list(id = c(et1 = 3, et2 = 3, et3 = Inf)))

  expect_equal(tmp2$lower, list(id = c(et1 = -Inf, et2 = -Inf, et3 = -Inf)))

  tmp2 <- lotri(
    et1 + et2 ~ c(
      1,
      2, 3
    ) | id(lower = c(et2 = 3)),
    et3 ~ 3 | id
  )

  expect_equal(
    tmp2$lower,
    list(id = c(et1 = -Inf, et2 = 3, et3 = -Inf))
  )

  tmp2 <- lotri(
    et1 + et2 ~ c(
      1,
      2, 3
    ) | id(upper = c(et2 = 3)),
    et3 ~ 3 | id
  )

  expect_equal(
    tmp2$upper,
    list(id = c(et1 = Inf, et2 = 3, et3 = Inf))
  )


  tmp2 <- lotri(
    eta.Cl ~ 0.1,
    eta.Ka ~ 0.2,
    inv.Cl ~ 0.3,
    inv.Ka ~ 0.4,
    iov.Ka ~ 0.5,
    iov.Cl ~ 0.6 | occ(lower = 3)
  )

  expect_equal(
    tmp2,
    structure(list(structure(c(
      0.1, 0, 0, 0, 0, 0, 0.2, 0,
      0, 0, 0, 0, 0.3, 0, 0, 0, 0,
      0, 0.4, 0, 0, 0, 0, 0, 0.5
    ),
    .Dim = c(5L, 5L),
    .Dimnames = list(
      c(
        "eta.Cl",
        "eta.Ka",
        "inv.Cl",
        "inv.Ka",
        "iov.Ka"
      ),
      c(
        "eta.Cl",
        "eta.Ka",
        "inv.Cl",
        "inv.Ka",
        "iov.Ka"
      )
    )
    ),
    occ = structure(0.6,
      .Dim = c(1L, 1L),
      .Dimnames = list(
        "iov.Cl",
        "iov.Cl"
      )
    )
    ),
    lotri = list(occ = list(lower = c(iov.Cl = 3))), class = "lotri"
    )
  )

  tmp2 <- lotri(
    inv.Ka ~ 0.4,
    iov.Ka ~ 0.5 | occ,
    iov.Cl ~ 0.6 | occ(lower = 3)
  )

  expect_equal(tmp2$lower$occ, c(iov.Ka = -Inf, iov.Cl = 3))


  tmp2 <- lotri(
    inv.Ka ~ 0.4,
    iov.Ka ~ 0.5 | occ(lower = 3),
    iov.Cl ~ 0.6 | occ
  )

  expect_equal(tmp2$lower$occ, c(iov.Ka = 3, iov.Cl = -Inf))

  tmp2 <- lotri(
    eta.Cl ~ 0.1,
    eta.Ka ~ 0.2,
    inv.Cl ~ 0.3,
    inv.Ka ~ 0.4 | occ,
    iov.Ka ~ 0.5,
    iov.Cl ~ 0.6 | occ(lower = 3)
  )

  expect_equal(tmp2$lower$occ, c(inv.Ka = -Inf, iov.Cl = 3))

  tmp2 <- lotri(
    eta.Cl ~ 0.1,
    eta.Ka ~ 0.2,
    inv.Cl ~ 0.3,
    inv.Ka ~ 0.4 | occ(lower = 3),
    iov.Ka ~ 0.5,
    iov.Cl ~ 0.6 | occ
  )

  expect_equal(tmp2$lower$occ, c(inv.Ka = 3, iov.Cl = -Inf))

  tmp2 <- lotri(lotri(
    iov.Ka ~ 0.5,
    iov.Cl ~ 0.6
  ) | occ(lower = 3))

  expect_equal(tmp2$lower, list(occ = c(iov.Ka = 3, iov.Cl = 3)))

  tmp2 <- lotri(
    lotri(
      iov.Ka ~ 0.5,
      iov.Cl ~ 0.6
    ) | iov(lower = 3),
    lotri(
      occ.Ka ~ 0.5,
      occ.Cl ~ 0.6
    ) | occ(lower = 4)
  )

  expect_equal(
    tmp2$lower,
    list(
      iov = c(iov.Ka = 3, iov.Cl = 3),
      occ = c(occ.Ka = 4, occ.Cl = 4)
    )
  )

  tmp2 <- lotri(
    lotri(
      iov.Ka ~ 0.5,
      iov.Cl ~ 0.6
    ) | iov(lower = 3),
    lotri(
      occ.Ka ~ 0.5,
      occ.Cl ~ 0.6
    ) | iov(lower = 4)
  )

  expect_equal(
    tmp2$lower,
    list(iov = c(
      iov.Ka = 3, iov.Cl = 3,
      occ.Ka = 4, occ.Cl = 4
    ))
  )


  tmp2 <- lotri(
    lotri(
      eta.Cl ~ 0.1,
      eta.Ka ~ 0.2
    ),
    lotri(
      inv.Ka ~ 0.3,
      inv.Cl ~ 0.4
    ) | inv(lower = 2),
    lotri(
      iov.Ka ~ 0.5,
      iov.Cl ~ 0.6
    ) | occ(lower = 3)
  )

  expect_equal(
    tmp2$lower,
    list(c(eta.Cl = -Inf, eta.Ka = -Inf),
      inv = c(inv.Ka = 2, inv.Cl = 2),
      occ = c(iov.Ka = 3, iov.Cl = 3)
    )
  )

  tmp2 <- lotri(lotri(
    iov.Ka ~ 0.5,
    iov.Cl ~ 0.6
  ) | occ)

  expect_equal(
    tmp2,
    list(occ = structure(c(0.5, 0, 0, 0.6),
      .Dim = c(2L, 2L),
      .Dimnames = list(
        c(
          "iov.Ka",
          "iov.Cl"
        ),
        c(
          "iov.Ka",
          "iov.Cl"
        )
      )
    ))
  )

  tmp2 <- lotri(
    lotri(
      iov.Ka ~ 0.5,
      iov.Cl ~ 0.6
    ) | iov,
    lotri(
      occ.Ka ~ 0.5,
      occ.Cl ~ 0.6
    ) | occ(lower = 4)
  )

  expect_equal(
    tmp2,
    structure(list(iov = structure(c(0.5, 0, 0, 0.6), .Dim = c(
      2L,
      2L
    ), .Dimnames = list(c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl"))), occ = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
      c("occ.Ka", "occ.Cl"), c("occ.Ka", "occ.Cl")
    ))), lotri = list(
      occ = list(lower = c(occ.Ka = 4, occ.Cl = 4))
    ), class = "lotri")
  )


  tmp2 <- lotri(
    lotri(
      iov.Ka ~ 0.5,
      iov.Cl ~ 0.6
    ) | iov(lower = 3),
    lotri(
      occ.Ka ~ 0.5,
      occ.Cl ~ 0.6
    ) | occ
  )

  expect_equal(
    tmp2,
    structure(list(iov = structure(c(0.5, 0, 0, 0.6), .Dim = c(
      2L,
      2L
    ), .Dimnames = list(c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl"))), occ = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
      c("occ.Ka", "occ.Cl"), c("occ.Ka", "occ.Cl")
    ))), lotri = list(
      iov = list(lower = c(iov.Ka = 3, iov.Cl = 3))
    ), class = "lotri")
  )

  tmp2 <- lotri(
    lotri(
      iov.Ka ~ 0.5,
      iov.Cl ~ 0.6
    ) | iov,
    lotri(
      occ.Ka ~ 0.5,
      occ.Cl ~ 0.6
    ) | occ
  )

  expect_equal(
    tmp2,
    list(iov = structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
      c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl")
    )), occ = structure(c(
      0.5,
      0, 0, 0.6
    ), .Dim = c(2L, 2L), .Dimnames = list(c("occ.Ka", "occ.Cl"), c("occ.Ka", "occ.Cl"))))
  )


  tmp2 <- lotri(
    lotri(
      iov.Ka ~ 0.5,
      iov.Cl ~ 0.6
    ),
    lotri(
      occ.Ka ~ 0.5,
      occ.Cl ~ 0.6
    ) | occ(lower = 4)
  )

  expect_equal(tmp2, structure(list(structure(c(0.5, 0, 0, 0.6), .Dim = c(2L, 2L), .Dimnames = list(
    c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl")
  )), occ = structure(c(
    0.5,
    0, 0, 0.6
  ), .Dim = c(2L, 2L), .Dimnames = list(c("occ.Ka", "occ.Cl"), c("occ.Ka", "occ.Cl")))), lotri = list(occ = list(lower = c(
    occ.Ka = 4,
    occ.Cl = 4
  ))), class = "lotri"))

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

  expect_equal(
    tmp2,
    structure(list(iov = structure(c(0.5, 0, 0, 0.6), .Dim = c(
      2L,
      2L
    ), .Dimnames = list(c("iov.Ka", "iov.Cl"), c("iov.Ka", "iov.Cl"))), 0.5, 0, 0, 0.6), lotri = list(iov = list(lower = c(
      iov.Ka = 3,
      iov.Cl = 3
    ))), class = "lotri")
  )
})

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

})

test_that("lotriMat", {

  tmp <- lotriMat(omega9)

  expect_error(lotriMatInv(omega9))

  tmp2 <- lotriMatInv(tmp)

  expect_equal(
    dimnames(tmp)[[1]],
    c(
      "eta.Cl", "eta.Ka", "eye.Cl", "eye.Ka", "iov.Cl",
      "iov.Ka", "inv.Cl", "inv.Ka"
    )
  )

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ fix(1,
              0.5, 1)
  })

  fix2 <- lotri({
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m+n ~ c(1,
            0.5, 1)
  })

  expect_equal(lotriMatInv(lotriEst(lotriMat(list(fix1, fix2)), drop=TRUE)),
               list(structure(c(1, 0.5, 0.5, 1), .Dim = c(2L, 2L),
                              .Dimnames = list(c("f", "g"), c("f", "g")),
                              lotriFix = structure(c(TRUE, TRUE, TRUE, TRUE),
                                                   .Dim = c(2L, 2L),
                                                   .Dimnames = list(c("f", "g"), c("f", "g"))),
                              class = .cls),
                    structure(c(1, 0.5, 0.5, 1), .Dim = c(2L, 2L),
                              .Dimnames = list(c("m", "n"), c("m", "n")),
                              lotriFix = structure(c(FALSE, FALSE, FALSE, FALSE),
                                                   .Dim = c(2L, 2L),
                                                   .Dimnames = list(c("m", "n"), c("m", "n"))),
                              class = .cls)))

  expect_equal(vapply(seq_along(tmp2), function(i) {
    dimnames(tmp2[[i]])[[1]]
  }, character(1)), c(
    "eta.Cl", "eta.Ka", "eye.Cl", "eye.Ka", "iov.Cl",
    "iov.Ka", "inv.Cl", "inv.Ka"
  ))



  expect_error(.Call(.lotri$`_asLotriMat`, "a", list(nu = 3), "id", PACKAGE = "lotri"))

  expect_error(.Call(.lotri$`_asLotriMat`, matrix(1), list(nu = 3), "id", PACKAGE = "lotri"))
  expect_error(.Call(.lotri$`_asLotriMat`, structure(1, .Dim = c(1L, 1L), dimnames = list(NULL, "a")),
    list(nu = 3), "id",
    PACKAGE = "lotri"
  ))
  expect_error(.Call(.lotri$`_asLotriMat`, structure(1, .Dim = c(1L, 1L), dimnames = list("a", NULL)),
    list(nu = 3), "id",
    PACKAGE = "lotri"
  ))

  expect_error(.Call(.lotri$`_asLotriMat`, lotri(et1 + et2 ~ c(0.1, 0.01, 1)),
    "a", "id",
    PACKAGE = "lotri"
  ))

  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = 1L, default = c("id", "id2")))
  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), upper = 1L, default = 3))

  expect_error(as.lotri(lotri(et1 + et2 ~ c(0.1, 0.01, 1)), lower = 4))

  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    lotri(et5 ~ 6),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2)
  )

  expect_equal(lotriMat(testList),
    structure(c(
      40, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 20, 0.1, 0, 0,
      0, 0, 0, 0.1, 0.1, 30, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0,
      0, 0, 0, 0, 0.1, 0.01, 0, 0, 0, 0, 0, 0, 0.01, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
    ), .Dim = c(8L, 8L)))

  testList <- list(
    matrix(c(1L, 0L, 0L, 1L), 2, 2),
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    lotri(et5 ~ 6),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1))
  )

  expect_equal(lotriMat(testList),
               structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                           0, 40, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 20, 0.1, 0, 0, 0, 0, 0,
                           0.1, 0.1, 30, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0,
                           0, 0.1, 0.01, 0, 0, 0, 0, 0, 0, 0.01, 1), .Dim = c(8L, 8L)))

  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    lotri(et5 ~ 6),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
      dimnames = list(
        c("et7", "et8"),
        c("et7", "et8")
      )
    )
  )

  expect_equal(
    lotriMat(testList),
    structure(c(
      40, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 20, 0.1, 0, 0,
      0, 0, 0, 0.1, 0.1, 30, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0,
      0, 0, 0, 0, 0.1, 0.01, 0, 0, 0, 0, 0, 0, 0.01, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
    ), .Dim = c(8L, 8L), .Dimnames = list(
      c("et2", "et3", "et4", "et5", "et1", "et6", "et7", "et8"),
      c("et2", "et3", "et4", "et5", "et1", "et6", "et7", "et8")
    ))
  )

  expect_error(lotriMat(list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    "A"
  )))

  expect_error(lotriMat(3))

  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 3),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
      dimnames = list(
        c("et7", "et8"),
        c("et7", "et8")
      )
    )
  )

  testList1 <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 3L),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
      dimnames = list(
        c("et7", "et8"),
        c("et7", "et8")
      )
    )
  )

  expect_equal(lotriMat(testList), lotriMat(testList1))

  expect_error(lotriMat(testList, 4))
  expect_error(lotriMat(testList, "eta[%d]", "a"))

  expect_equal(
    dimnames(lotriMat(testList, "ETA[%d]", start = 3))[[1]],
    c(
      "et2", "et3", "et4", "ETA[3]", "ETA[4]", "ETA[5]",
      "et1", "et6", "et7", "et8"
    )
  )

  expect_equal(
    dimnames(lotriMat(testList, "ETA[%d]"))[[1]],
    c(
      "et2", "et3", "et4", "ETA[1]", "ETA[2]", "ETA[3]",
      "et1", "et6", "et7", "et8"
    )
  )

  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 3, 4),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
      dimnames = list(
        c("et7", "et8"),
        c("et7", "et8")
      )
    )
  )
  expect_error(lotriMat(testList))

  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 0),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
      dimnames = list(
        c("et7", "et8"),
        c("et7", "et8")
      )
    )
  )
  expect_error(lotriMat(testList))

  testList <- list(
    lotri({
      et2 + et3 + et4 ~ c(
        40,
        0.1, 20,
        0.1, 0.1, 30
      )
    }),
    list(lotri(et5 ~ 6), 1:3),
    lotri(et1 + et6 ~ c(0.1, 0.01, 1)),
    matrix(c(1L, 0L, 0L, 1L), 2, 2,
      dimnames = list(
        c("et7", "et8"),
        c("et7", "et8")
      )
    )
  )

  expect_error(lotriMat(testList))

  mat1 <- lotri({
    et2 + et3 + et4 ~ c(
      40,
      0.1, 20,
      0.1, 0.1, 30
    )
  })

  expect_equal(mat1, lotriMat(mat1))

  mat1 <- list(mat1, 3)

  expect_equal(lotriMat(mat1), lotriMat(list(mat1)))
  expect_equal(lotriMat(mat1, "ETA[%d]"), lotriMat(list(mat1), "ETA[%d]"))
  expect_equal(lotriMat(mat1, "ETA[%d]", 4), lotriMat(list(mat1), "ETA[%d]", 4L))

})

test_that("lotriSep", {

  sep0 <- lotriSep(omega9, above = c(inv = 10L), below = c(eye = 2L, occ = 4L))

  attr(sep0$below, "format") <- "ETA[%d]"
  attr(sep0$below, "start") <- 1L

  attr(sep0$above, "format") <- "THETA[%d]"
  attr(sep0$above, "start") <- 1L


  sepA <- lotriSep(omega, above = c(inv = 10L), below = c(eye = 2L, occ = 4L))

  sepB <- list(
    above = lotri(lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) |
      inv(nu = 100, same = 10L)),
    below = lotri(
      lotri(
        eta.Cl ~ 0.1,
        eta.Ka ~ 0.1
      ) | id(nu = 100),
      lotri(
        eye.Cl ~ 0.05,
        eye.Ka ~ 0.05
      ) | eye(nu = 50, same = 2L),
      lotri(
        iov.Cl ~ 0.01,
        iov.Ka ~ 0.01
      ) | occ(nu = 200, same = 4L)
    )
  )

  attr(sepB$below, "format") <- "ETA[%d]"
  attr(sepB$below, "start") <- 1L

  attr(sepB$above, "format") <- "THETA[%d]"
  attr(sepB$above, "start") <- 1L

  expect_equal(sepA, sepB)

  expect_equal(
    dimnames(lotriMat(sepA$above))[[1]],
    sprintf("THETA[%d]", 1:20)
  )
  expect_equal(
    dimnames(lotriMat(sepA$below))[[1]],
    c("eta.Cl", "eta.Ka", sprintf("ETA[%d]", 1:12))
  )

  above1 <- attr(sepA$above, "lotri")
  above1$inv$same <- "matt"
  above <- sepA$above
  attr(above, "lotri") <- above1

  expect_equal(dimnames(lotriMat(above))[[1]], c("inv.Cl", "inv.Ka"))

  expect_error(lotriSep(omega, above = c(inv = 10L), below = c(eye = 2L, occ = 4L), aboveStart = 1:2))

  expect_error(lotriSep(omega, above = c(inv = 10), below = c(eye = 2L, occ = 4L)))

  expect_error(lotriSep(omega, above = c(inv = 10L), below = c(eye = 2, occ = 4)))

  expect_error(lotriSep(omega, above = 10L, below = c(eye = 2L, occ = 4L)))

  expect_error(lotriSep(omega, above = c(inv = 10L), below = c(2L, 4L)))

  expect_error(lotriSep(omega, above = c(inv = 10L), below = c(eye = 2L, matt = 4L), aboveStart = 2L))

  omega0 <- lotri(
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

  expect_error(lotriSep(omega0, above = c(inv = 10L), below = c(eye = 2L, occ = 4L), aboveStart = 2L))

  sepA <- lotriSep(omega, above = NULL, below = c(eye = 2L, occ = 4L))

  expect_equal(sepA$above, NULL)

  sepA <- lotriSep(omega, above = NULL, below = c(eye = 2L, occ = 4L))

  expect_equal(sepA$above, NULL)

  ## Bad Lotri matrix

  omega1 <- structure(list(id = structure(c(0.1, 0, 0, 0.1), .Dim = c(
    2L,
    2L
  ), .Dimnames = list(c("eta.Cl", "eta.Ka"), c("eta.Cl", "eta.Ka"))), eye = structure(c(0.05, 0, 0, 0.05), .Dim = c(2L, 2L), .Dimnames = list(
    c("eye.Cl", "eye.Ka"), c("eye.Cl", "eye.Ka")
  )), occ = structure(c(
    0.01,
    0, 0, 0.01
  ), .Dim = c(2L, 2L), .Dimnames = list(c("iov.Cl", "iov.Ka"), c("iov.Cl", "iov.Ka"))), inv = structure(c(0.02, 0, 0, 0.02), .Dim = c(2L, 2L), .Dimnames = list(
    c("inv.Cl", "inv.Ka"),
    c("inv.Cl", "inv.Ka")
  ))), lotri = list(
    id = list(nu = 100),
    eye = list(nu = 50), inv = list(nu = 10)
  ), class = "lotri")

  expect_error(lotriSep(omega1, above = NULL, below = c(eye = 2L, occ = 4L)))

})
test_that("allNames", {

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

  expect_equal(
    omega$.allNames,
    c(
      "inv.Cl", "inv.Ka", "iov.Cl", "iov.Ka",
      "eye.Cl", "eye.Ka", "eta.Cl", "eta.Ka"
    )
  )

  expect_error(.Call(.lotri$`_lotriAllNames`, 1:20, PACKAGE = "lotri"))


  mat0 <- lotri(
    eta.Cl ~ 0.1,
    eta.Ka ~ 0.1
  )
  dn <- dimnames(mat0)
  dn0 <- list(dn[[1]], NULL)
  dimnames(mat0) <- dn0

  dn1 <- .Call(.lotri$`_lotriAllNames`, mat0, PACKAGE = "lotri")

  dn0 <- list(NULL, dn[[1]])
  dimnames(mat0) <- dn0

  dn2 <- .Call(.lotri$`_lotriAllNames`, mat0, PACKAGE = "lotri")

  expect_equal(dn1, dn2)

  dn0 <- list(NULL, NULL)
  dimnames(mat0) <- dn0

  dn3 <- .Call(.lotri$`_lotriAllNames`, mat0, PACKAGE = "lotri")

  expect_equal(dn3, character(0))

  dimnames(mat0) <- NULL

  dn3 <- .Call(.lotri$`_lotriAllNames`, mat0, PACKAGE = "lotri")

  expect_equal(dn3, character(0))

  name9 <- c(
    "inv.Cl", "inv.Ka", "iov.Cl", "iov.Ka", "eye.Cl", "eye.Ka",
    "eta.Cl", "eta.Ka"
  )
  expect_equal(
    .Call(.lotri$`_lotriAllNames`, omega9, PACKAGE = "lotri"),
    name9
  )

})
test_that("bounds C", {

  tmp <- .Call(.lotri$`_lotriGetBounds`, omega9, NULL, NULL, PACKAGE = "lotri")
  expect_true(all(!is.finite(tmp$lower)))
  expect_true(all(!is.finite(tmp$upper)))

  omega <- lotri(
    lotri(
      eta.Cl ~ 0.1,
      eta.Ka ~ 0.1
    ) | id(nu = 100, lower = 3, upper = 4),
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye(nu = 50, lower = c(eye.Cl = 4)),
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ(nu = 200),
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv(nu = 10)
  )

  expect_equal(omega$upper$id, c(eta.Cl = 4, eta.Ka = 4))
  expect_equal(omega$lower$id, c(eta.Cl = 3, eta.Ka = 3))

  lst <- omega$.bounds

  expect_equal(lst$lower, c(
    eta.Cl = 3, eta.Ka = 3, eye.Cl = 4, eye.Ka = -Inf, iov.Cl = -Inf,
    iov.Ka = -Inf, inv.Cl = -Inf, inv.Ka = -Inf
  ))

  expect_equal(lst$upper, c(
    eta.Cl = 4, eta.Ka = 4, eye.Cl = Inf, eye.Ka = Inf, iov.Cl = Inf,
    iov.Ka = Inf, inv.Cl = Inf, inv.Ka = Inf
  ))


  omega <- lotri(
    lotri(
      eta.Cl ~ 0.1,
      eta.Ka ~ 0.1
    ) | id(nu = 100, lower = 3, upper = 4),
    lotri(
      eye.Cl ~ 0.05,
      eye.Ka ~ 0.05
    ) | eye(nu = 50, lower = c(eye.Cl = 4)),
    lotri(
      iov.Cl ~ 0.01,
      iov.Ka ~ 0.01
    ) | occ(nu = 200),
    lotri(
      inv.Cl ~ 0.02,
      inv.Ka ~ 0.02
    ) | inv(nu = 10)
  )

  sepA <- lotriSep(omega, above = c(inv = 10L), below = c(eye = 2L, occ = 4L))

  lst <- sepA$above$.bounds
  expect_equal(names(lst$lower), sprintf("THETA[%d]", 1:20))
  expect_equal(names(lst$upper), sprintf("THETA[%d]", 1:20))

  lst <- sepA$below$.bounds

  expect_equal(c(
    3, 3, 4, -Inf, 4, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf,
    -Inf, -Inf
  ), as.vector(lst$lower))

  expect_equal(c(
    4, 4, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,
    Inf
  ), as.vector(lst$upper))

  expect_equal(names(lst$upper), c("eta.Cl", "eta.Ka", sprintf("ETA[%d]", 1:12)))

  above <- sepA$above

  lotriProp <- attr(above, "lotri")
  lotriProp$inv$same <- 10L

  above2 <- above
  attr(above2, "lotri") <- lotriProp

  expect_equal(lotriMat(above), lotriMat(above2))

  lotriProp$inv$lower <- 3L
  above3 <- above
  attr(above3, "lotri") <- lotriProp

  expect_equal(as.vector(above3$.bounds$lower), rep(3.0, 20))

  lotriProp$inv$lower <- 3.0
  above4 <- above
  attr(above4, "lotri") <- lotriProp

  expect_equal(as.vector(above4$.bounds$lower), rep(3.0, 20))

  lotriProp$inv$lower <- c(3, 4)
  above5 <- above
  attr(above5, "lotri") <- lotriProp

  expect_error(above5$.bounds)

  expect_error(.Call(.lotri$`_lotriGetBounds`, lotri(a ~ 3), NULL, 1, PACKAGE = "lotri"))

  lotriProp$inv$lower <- c(inv.Cl = 3L, inv.Ka = 3L)
  above7 <- above
  attr(above7, "lotri") <- lotriProp


  expect_equal(as.vector(above4$.bounds$lower), rep(3.0, 20))

  expect_error(.Call(.lotri$`_lotriGetBounds`, "A", NULL, 1, PACKAGE = "lotri"))

})
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

test_that("transformations", {

  expect_equal(lotri(s1 + s2 + s3 ~ cor(sd(1,
                                           0.25, 4,
                                           0.90, 0.50, 9))),
               lotri(s1 + s2 + s3 ~ sd(cor(1,
                                           0.25, 4,
                                           0.90, 0.50, 9))))

  expect_equal(lotri(s1 + s2 + s3 ~ cor(sd(1,
                                           0.25, 4,
                                           0.90, 0.50, 9))),
               lotri(s1 + s2 + s3 ~ c(1,
                                      1, 16,
                                      8.1, 18, 81)))

  expect_equal(lotri(s1 + s2 + s3 ~ cor(1,
                                           0.25, 4,
                                           0.90, 0.50, 9)),
               lotri(s1 + s2 + s3 ~ c(1,
                                      0.5, 4.0,
                                      2.7, 3.0, 9.0)))

  expect_error(lotri(s1 + s2 + s3 ~ cor(sd(1,
                                           2, 4,
                                           0.90, 0.50, 9))))


  expect_error(lotri(s1 + s2 + s3 ~ sd(var(1,
                                           0.5, 4,
                                           0.90, 0.50, 9))))

  expect_equal(diag(lotri(s1 + s2 + s3 ~ var(1,
                                             0.5, 4,
                                             0.90, 0.50, 9))),
               c(s1=1, s2=4, s3=9))


  ## Cholesky
  m <- matrix(c(2.2,0.4,0,1.6),2,2)

  m2 <- m %*% t(m)

  m3 <- lotri(s1 + s2 ~ chol(2.2,
                             0.4, 1.6))
  dimnames(m2) <- dimnames(m3)

  expect_equal(m2, m3)


  expect_error(lotri(s1 + s2 ~ sd(chol(2.2,
                                       0.4, 1.6))))

  expect_error(lotri(s1 + s2 ~ sd(var(2.2,
                                      0.4, 1.6))))

  expect_error(lotri(s1 + s2 ~ cov(cor(2.2,
                                       0.4, 1.6))))

  expect_error(lotri(s1 + s2 ~ cor(cov(2.2,
                                       0.4, 1.6))))

})
test_that("fixed tests", {

  tmp <- lotri(lotri(a + b ~ fix(0.1,
                                 0.001, 0.1)),
               lotri(c + d ~ c(0.1,
                               0.001, 0.1)))

  expect_equal(attr(tmp, "lotriFix"),
               structure(c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(4L, 4L), .Dimnames = list(c("a", "b", "c", "d"), c("a", "b", "c", "d"))))

  tmp <- lotri(a + b + c ~ c(
    fix(40),
    0.1, 20,
    0.1, 0.1, 30
  ))

  expect_equal(attr(tmp, "lotriFix"),
               structure(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(3L, 3L), .Dimnames = list(c("a", "b", "c"),     c("a", "b", "c"))))

  tmp <- lotri(a + b + c ~ c(
    40,
    fixed(0.1), 20,
    0.1, 0.1, 30
  ))

  expect_equal(attr(tmp, "lotriFix"),
               structure(c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(3L, 3L), .Dimnames = list(c("a", "b", "c"),     c("a", "b", "c"))) )

  tmp <- lotri(a + b + c ~ c(
    40,
    0.1, 20,
    fix(0.1), 0.1, 30
  ))

  expect_equal(attr(tmp, "lotriFix"),
               structure(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE), .Dim = c(3L, 3L), .Dimnames = list(c("a", "b", "c"),     c("a", "b", "c"))))

  expect_snapshot_output(print(tmp))

})
test_that("Combined estimates and matrix", {

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
  })

  expect_snapshot_output(print(fix1))

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ c(1,
            0.5, 1)
  })

  expect_snapshot_output(print(fix1))

  expect_equal(attr(fix1, "lotriEst"),
               structure(list(name = c("a", "b", "c", "d", "e"),
                              lower = c(0, 0, -Inf, 0, 0),
                              est = c(1, 1, 1, 1, 1),
                              upper = c(Inf, 2, Inf, 2, 2),
                              fix = c(FALSE, FALSE, TRUE, TRUE, TRUE),
                              label = c("a label",NA, NA, NA, NA),
                              backTransform = c("exp", NA, NA, NA, NA)),
                         row.names = c(NA,-5L), class = "data.frame"))
  fix2 <- fix1
  attr(fix2, "lotriEst") <- NULL
  class(fix2) <- NULL

  expect_equal(fix2,
               structure(c(1, 0.5, 0.5, 1),
                         .Dim = c(2L, 2L),
                         .Dimnames = list(c("f", "g"), c("f", "g"))))

  fix2 <- lotri({
    a = c(3); label(matt); backTransform(exp) #nolint
  })

  expect_equal(attr(fix2, "lotriEst"),
               structure(list(name = "a",
                              lower = -Inf,
                              est = 3,
                              upper = Inf,
                              fix = FALSE,
                              label = "matt",
                              backTransform = "exp"),
                         row.names = c(NA, -1L),
                         class = "data.frame"))

  expect_error(lotri({a = "matt"}))

  expect_error(lotri({
    a = c(1, 2, 3, 4)
    b <- c(NA) # nolint
    c <- c(NA, NA, NA)
    d <- c(NaN) #nolint
    e <- c(NaN, NaN, NaN)
    f <- Inf
    g <- c(Inf, 1, 2)
    h <- c(0, 1, -Inf)
    i <- c(1, 1, 1)
    j <- c(3, 2, 1)
  }))

  # Don't allow dupliate parameters with a mixed matrix/estimate
  expect_error(lotri({b=3;b~0.4}))


})
test_that("combine fix1 and fix2", {

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ c(1,
            0.5, 1)
  })

  fix2 <- lotri({
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m+n ~ c(1,
            0.5, 1)
  })


  c1 <- lotriMat(list(fix1, fix2))

  expect_error(lotriMatInv(c1))

  expect_equal(attr(c1, "lotriEst"),
               structure(list(name = c("a", "b", "c", "d", "e", "h", "i", "j", "k", "l"),
                              lower = c(0, 0, -Inf, 0, 0, 0, 0, -Inf, 0, 0),
                              est = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                              upper = c(Inf, 2, Inf, 2, 2, Inf, 2, Inf, 2, 2),
                              fix = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
                              label = c("a label", NA, NA, NA, NA, "b label", NA, NA, NA, NA),
                              backTransform = c("exp", NA, NA, NA, NA, "expit", NA, NA, NA, NA)),
                         class = "data.frame", row.names = c(NA, 10L)))

  class(c1) <- NULL
  attr(c1, "lotriEst") <- NULL

  expect_equal(c1, structure(c(1, 0.5, 0, 0, 0.5, 1, 0, 0, 0, 0, 1, 0.5, 0, 0, 0.5, 1),
                             .Dim = c(4L, 4L),
                             .Dimnames = list(c("f", "g", "m", "n"), c("f", "g", "m", "n"))))


  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ c(1,
            0.5, 1)
  })

  fix2 <- lotri({
    m+n ~ c(1,
            0.5, 1)
  })

  c1 <- lotriMat(list(fix1, fix2))

  expect_equal(lotriEst(c1),
               structure(list(name = c("a", "b", "c", "d", "e"),
                              lower = c(0, 0, -Inf, 0, 0),
                              est = c(1, 1, 1, 1, 1),
                              upper = c(Inf, 2, Inf, 2, 2),
                              fix = c(FALSE, FALSE, TRUE, TRUE, TRUE),
                              label = c("a label", NA, NA, NA, NA),
                              backTransform = c("exp", NA, NA, NA, NA)),
                         class = "data.frame", row.names = c(NA, 5L)))

  expect_equal(lotriEst(c1, drop=TRUE),
               structure(c(1, 0.5, 0, 0, 0.5, 1, 0, 0, 0, 0, 1, 0.5, 0, 0, 0.5, 1),
                         .Dim = c(4L, 4L),
                         .Dimnames = list(c("f", "g", "m", "n"), c("f", "g", "m", "n"))))

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
  })

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ fix(1,
              0.5, 1)
  })

  fix2 <- lotri({
    m+n ~ c(1,
            0.5, 1)
  })

  c1 <- lotriMat(list(fix1, fix2))

  expect_true(inherits(lotriEst(c1, drop=TRUE), "lotriFix"))

})
test_that("as.data.frame", {

  fix1 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ fix(1,
              0.5, 1)
  })

  fix2 <- lotri({
    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m+n ~ c(1,
            0.5, 1)
  })


  c1 <- lotriMat(list(fix1, fix2))

  expect_equal(as.data.frame(c1),
               structure(list(ntheta = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, NA, NA, NA, NA, NA, NA),
                              neta1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L, 2L, 2L, 1L, 2L, 2L), neta2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L, 1L, 2L, 1L, 1L, 2L),
                              name = c("a", "b", "c", "d", "e", "h", "i", "j", "k", "l", "f", "(f,g)", "g", "m", "(m,n)", "n"),
                              lower = c(0, 0, -Inf, 0, 0, 0, 0, -Inf, 0, 0, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf), est = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 1, 1, 0.5, 1),
                              upper = c(Inf, 2,  Inf, 2, 2, Inf, 2, Inf, 2, 2, Inf, Inf, Inf, Inf, Inf, Inf), fix = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
                              label = c("a label", NA, NA, NA, NA, "b label", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                              backTransform = c("exp", NA, NA, NA, NA, "expit", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                              condition = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "ID", "ID", "ID", "ID", "ID", "ID")),
                         class = "data.frame", row.names = c(NA, -16L)))


  expect_snapshot_output(print(fix1))



  fix2 <- lotri({
    a <- c(0, 1); backTransform("exp"); label("a label")
    b <- c(0, 1, 2)
    c <- fix(1)
    d <- fix(0, 1, 2)
    e <- c(0, 1, 2, fixed)
    f+g ~ fix(1,
              0.5, 1) | occ

    h <- c(0, 1); backTransform("expit"); label("b label")
    i <- c(0, 1, 2)
    j <- fix(1)
    k <- fix(0, 1, 2)
    l <- c(0, 1, 2, fixed)
    m+n ~ c(1,
            0.5, 1)
  })

  ## c1 <- lotriMat(list(fix1, fix2))

})

