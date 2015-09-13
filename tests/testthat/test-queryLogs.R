context("Advanced queries to get logs")

test_that("Get custom steam profile name from SteamID3",{
	testPlayer <- "[U:1:2431135]"
	customName <- convSID2name(testPlayer)

	expect_true(grepl("ryb", customName))
})

test_that("notUnique actually does && right", {
	vec <- c(2, rep(1,3), rep(5,2), 7, rep(9, 3))
	niceVec <- notUnique(vec)
	expect_equal(niceVec, c(1, 5, 9))
	expect_false(as.logical(anyDuplicated(niceVec)))
})

test_that("notUnique falls through with a unique set", {
	vec <- 1:10
	niceVec <- notUnique(vec)
	expect_equal(niceVec, vec)
})
