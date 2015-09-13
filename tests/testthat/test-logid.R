context("Log ID")
test_that("Gets right Ids given a Player", {
  testPlayer <- "[U:1:72806494]" #Herr_p

  logID <- getLogIDsJSON(player = testPlayer, num = 2)
  for(lid in paste0("http://logs.tf/", logID)){
    test <- xml2::read_html(lid)
    expect_false(grepl("Not Found", test))
  }

})

test_that("comp.tf scraping gets Tourney IDs right",{
	logIDs <- getLogIDsComptf("Insomnia52", withNames = FALSE, shReDownload = TRUE)

	expect_equal(length(logIDs), 52)
	expect_true(is.character(logIDs))
})

test_that("comp.tf scraping gets Tourney IDs right",{
	logIDs <- getLogIDsComptf("Insomnia52", withNames = TRUE, shReDownload = TRUE)

	expect_equal(length(logIDs), 52)
	expect_true(is.character(logIDs))
	expect_true(any(grepl("Grand", names(logIDs))))
})
