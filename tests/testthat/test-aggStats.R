context("Integration with aggregate stats")

#testPlayer <- "[U:1:72806494]" #Herr_p
#logID <- getLogIDsJSON(player = testPlayer, num = 2)
#logs <- lapply(logID, getLog)
load("llogs.RData")
test_that("Agg Stats does mean", {
  meanLog <- aggregateStats(logs)
  testName <- rownames(meanLog)[1]
  verboseMethod <- c(
    logs[[1]]$table[, "dmg"][testName],
    logs[[2]]$table[, "dmg"][testName])
  verboseMethod <- mean(verboseMethod)
  niceMethod <- as.double(meanLog[, "dmg"][testName])

  expect_equal(niceMethod, verboseMethod, tolerance = 1)

})
