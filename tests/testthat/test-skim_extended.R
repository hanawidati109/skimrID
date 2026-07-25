test_that("skim_extended mengembalikan data frame", {

  hasil <- skim_extended(iris)

  expect_s3_class(
    hasil,
    "data.frame"
  )

})
