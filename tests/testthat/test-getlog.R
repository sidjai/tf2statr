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
  alt <- list(`[U:1:72806494]` = "Herr_p")
  lgwnames <- getLog(testLogId, altNames = alt)

  nam <- rownames(lgwnames$table)
  ind <- match("Herr_p", nam)
  expect_false(is.na(ind))

  befInd <- match("[U:1:72806494]", rownames(lgtf$table))
  expect_equal(befInd, ind)

})
