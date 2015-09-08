context("Advanced queries to get logs")

test_that("Get steam ID from tf.tv username",{
  qs <- cbind("","","")
  addPlayerQs(qs) <- "LuckyLuke"
  expect_equal(dim(qs)[1], 2)
  expect_equal(qs[2,3], "[U:1:23274906]")
  expect_equal(qs[2,1], "")
  expect_equal(qs[2,2], "")

})

test_that("notUnique actually does && right", {
  vec <- c(2, rep(1,3), rep(5,2), 7, rep(9, 3))
  niceVec <- notUnique(vec)
  expect_equal(niceVec, c(1, 5, 9))
  expect_false(as.logical(anyDuplicated(niceVec)))
})
