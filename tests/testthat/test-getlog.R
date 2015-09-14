context("Get Log")

testLogId <- 998128 #Ascent TLR i55 LB
lgtf <- getLog(testLogId)

test_that("Parses log correctly", {


  expect_true(is.double(lgtf$table))
  expect_false(is.null(colnames(lgtf$table)))
  expect_false(is.null(rownames(lgtf$table)))

  leftOverComplex <- vapply(lgtf$players[[1]], function(x){
    is.data.frame(x) | is.list(x)
  }, TRUE)
  expect_true(all(leftOverComplex))
})

test_that("Alt names are done right", {

  lgwnames <- getLog(testLogId, useAltNames = TRUE)

  nam <- rownames(lgwnames$table)
  ind <- match("The7alfa", nam)
  expect_false(is.na(ind))

  befInd <- match("[U:1:38625038]", rownames(lgtf$table))
  expect_equal(befInd, ind)

})

test_that("Corrupted games are recognized", {
  badID <- "1000714"

  badLog <- getLog(badID)

  expect_true(all(is.na(badLog$table)))
})
