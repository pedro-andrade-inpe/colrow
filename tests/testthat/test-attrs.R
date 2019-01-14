context("attrs")

test_that("create", {
  attrs<-colrow::attrs(COUNTRY,ID,ALTICLASS,SLPCLASS,SOILCLASS,AEZCLASS,CROP,CROPTECH,SSP,MITSCEN,
                       CLIMSCEN,YEAR,VALUE)

  expect_equal(length(attrs), 13)

  expect_error(attrs<-colrow::attrs(YEAR,VALUE2), "There should exist one attribute called 'VALUE'.")
})
