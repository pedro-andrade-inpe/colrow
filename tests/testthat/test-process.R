context("process")

test_that("process", {

  expect_error(processFile(NULL, NULL, function(){}),
               "Argument 'description' should be 'character', got 'function'.")
})
