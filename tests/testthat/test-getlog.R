context("Get Log")

test_that("Parses log correctly", {
  testLogId <- 998128 #Ascent TLR i55 LB
  lgtf <- getLog(testLogId)

  expect_true(is.double(lgtf$table))
  expect_false(is.null(colnames(lgtf$table)))
  expect_false(is.null(rownames(lgtf$table)))

  leftOverComplex <- vapply(lgtf$players[[1]], function(x){
    is.data.frame(x) | is.list(x)
  }, TRUE)
  expect_true(all(leftOverComplex))
})
