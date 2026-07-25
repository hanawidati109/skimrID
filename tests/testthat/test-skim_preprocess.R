test_that("skim_preprocess menghasilkan output yang benar", {

  hasil <- skim_preprocess(airquality)

  expect_type(
    hasil,
    "list"
  )

  expect_true(
    all(c("data","detail","summary") %in% names(hasil))
  )

})
