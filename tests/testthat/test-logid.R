context("Log ID")
test_that("Gets right Ids given a Player", {
  testPlayer <- "[U:1:72806494]" #Herr_p

  logID <- getLogIDsJSON(player = testPlayer, num = 2)
  for(lid in paste0("http://logs.tf/", logID)){
    test <- xml2::read_html(lid)
    expect_false(grepl("Not Found", test))
  }

})
