context("Archiving")

test_that("Get name archives", {
	expect_true(grepl("[U:1:2431135]", getArchiveSIDfromName("ryb")))
})

test_that("playerDict archive is healthy", {
	data("playerDict")

	playerDict <- as.matrix(playerDict)
	expect_true(is.character(playerDict))
	expect_false(any(grepl("\\[", playerDict[, 1])))
	expect_true(all(grepl("\\[", playerDict[, 2])))

})

test_that("eventArchive data set is healthy", {
	data("eventArchive")

	expect_true(is.list(events))

	eleList <- vapply(events, is.list, TRUE)
	expect_true(all(eleList))

	eleNames <- vapply(events, function(x){
		!is.null(names(x)) && all(nzchar(x))
	}, TRUE)
	expect_true(all(eleNames))
})
