context("Log ID")
test_that("Gets right Ids given a Player", {
  testPlayer <- "[U:1:72806494]" #Herr_p

  logID <- getLogIDs(player = testPlayer, num = 2)
  for(lid in paste0("http://logs.tf/", logID)){
  	con <- url(lid)
  	corrupt <- suppressWarnings(readLines(con))
  	close(con)
    expect_true(length(corrupt) > 5)
  }

})
