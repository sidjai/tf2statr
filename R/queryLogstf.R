queryLogstf <- function(
	players = c(),
	teamName = "",
	season = "",
	tournament = "",
	shGetLog = TRUE){

	queries <- matrix(nrow = 1, ncol = 3)
	colnames(queries) <- c("title", "uploader", "player")
	ids <- c()

	if(length(players) > 0){
		playDict <- vapply(players, function(x){
			tftvUser2SteamID(x)
		}, "e")
		addPlayerQs(queries) <- playDict
	}

	if(length(season) > 0){
		addSeasonQs(queries) <- season
	}

	if(length(tournament) > 0){
		ids <- c(ids, getLogIDsComptf(tournament, "Tourney"))
	}

	if(dim(queries)[1] + length(ids) == 1 ){
		stop(paste("Please supply a list of",
		"players, the team name or the tournament to query the logs"))
	} else { queries <- queries[-1,] }

	ids <- c(ids, getLogIDsJSON(
		title = queries[, "title"],
		uploader = queries[, "uploader"],
		player = queries[, "player"],
		num = 10))

	andIds <- notUnique(ids)

	if(shGetLog){
		llogs <- lapply(andIds, getLog, altNames = playDict)
		return(llogs)
	} else {
		return(ids)
	}

}

`addPlayerQs<-` <- function(qs, value){
	return(rbind(qs, cbind("", "", value)))
}

tftvUser2SteamID <- function(tftvUserName){
	tftvurl <- paste0("http://www.teamfortress.tv/user/", tftvUserName)
	xpathIDs <- '//*[@id="content-inner"]/div[1]/table[1]'

	node <- easyScrape(tftvurl, xpathIDs, "Page Not Found")
	tftable <- rvest::html_table(node)
	sid3 <- tftable[tftable[,1] == "SteamID3", 2]

	names(sid3) <- tftvUserName
	return(sid3)
}

convSID2name <- function(sid, saveArchive = TRUE){

	data("playerDict", package = "tf2statr")
	playerDict <- as.matrix(playerDict)

	idenNum <- match(sid, playerDict[, 2])
	convName <- rep("", length(sid))

	useDictSet <- !is.na(idenNum)
	convName[useDictSet] <- playerDict[idenNum[useDictSet], 1]

	queries <- paste0("http://steamcommunity.com/profiles/", sid[!useDictSet])
	realProfiles <- twitteR::decode_short_url(queries)
	realProfiles <- gsub("http://steamcommunity.com/id/", "", realProfiles)
	convName[!useDictSet] <- gsub("[/]", "", realProfiles)

	if(saveArchive && !all(useDictSet)){
		addMat <- cbind(convName, sid)[!useDictSet, , drop = FALSE]
		write.table(
			addMat,
			file = system.file("data", "playerDict.csv", package = "tf2statr"),
			append = TRUE, row.names = FALSE, col.names = FALSE, sep = ";")

	}

	return(convName)

}

easyScrape <- function(url, xpathCap, failRegex){
	htmlFile <- xml2::read_html(url)
	if( grepl(failRegex, htmlFile) ){
		stop(paste0("'", value, "'", " is not a real web page/search (404)"))
	}
	node <- rvest::html_node(htmlFile, xpath = xpathCap)
	return(node)
}

`addSeasonQs<-` <- function(qs, value){
	#Go through league uploads
}

notUnique <- function(vec){
	dupSet <- duplicated(vec) | duplicated(vec, fromLast = TRUE)
	return(unique(vec[dupSet]))
}
