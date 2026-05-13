test_that(".lotriPointers returns 10 external pointers", {
  ptrs <- .lotriPointers()
  expect_equal(length(ptrs), 10L)
  expect_equal(names(ptrs),
               c("lotriLstToMat", "asLotriMat", "lotriSep", "lotriAllNames",
                 "lotriGetBounds", "lotriMaxNu", "isLotri", "lotriRcm",
                 "lotriNearPDc", "lotriNearPDsexp"))
})
