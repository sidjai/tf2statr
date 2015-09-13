context("Archiving")

test_that("Get name archives", {
	expect_true(grepl("[U:1:2431135]", getArchiveSIDfromName("ryb")))
})
