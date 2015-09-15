#'Do complicated queries of TF2stats
#'
#'Builds a list of logs that are relevant for a query using the union of all the
#'log IDs for the individual queries. The queries that can be done right now are
#'player queries using log.tf Search tool as well as season and tournament using
#'comp.tf historical pages. The players can be inputed as a list of Steam ID3s
#'or custom profile names which are then converted into steam ID3s. Whenever
#'possible, the program uses the archive to find log IDs for events.
#'@param players A vector of players either given as Steam ID3s or custom
#'  profile names
#'@param teamName TODO
#'@param season The string of the comp.tf webpage end piece (Insomnia52)
#'@param tournament The string of the comp.tf webpage end piece (Insomnia52)
#'@param shGetLog Should the program go ahead and get the logs of the query or
#'  just output the log IDs.
#'
#'@return Either a vector of log IDs if shGetLog is false, or a list of the
#'  relevant logs
#'@export
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
		addPlayerQs(queries) <- players
	}

	if(nzchar(season)){
		addSeasonQs(queries) <- season
	}

	if(nzchar(tournament)){
		ids <- c(ids, getLogIDsComptf(tournament, "Tourney"))
	}

	if(dim(queries)[1] + length(ids) == 1 ){
		stop(paste("Please supply a list of",
		"players, the team name or the tournament to query the logs"))
	} else if(dim(queries)[1] > 1){
		queries <- queries[-1,]
		ids <- c(ids, getLogIDsJSON(
			title = queries[, "title"],
			uploader = queries[, "uploader"],
			player = queries[, "player"],
			num = 10))
	}



	andIds <- notUnique(ids)

	if(shGetLog){
		llogs <- lapply(andIds, function(x){
			getLog(x, useAltNames = TRUE)
		})
		return(llogs)
	} else {
		return(ids)
	}

}

`addPlayerQs<-` <- function(qs, value){
	sid <- getArchiveSIDfromName(value)
	return(rbind(qs, cbind("", "", sid)))
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

	data("playerDict", package = "tf2statr", envir = environment())
	playerDict <- as.matrix(playerDict)

	idenNum <- match(sid, playerDict[, 2])
	convName <- rep("", length(sid))

	useDictSet <- !is.na(idenNum)
	convName[useDictSet] <- playerDict[idenNum[useDictSet], 1]

	queries <- paste0("http://steamcommunity.com/profiles/", sid[!useDictSet])
	realProfiles <- vapply(queries, function(x){
		twitteR::decode_short_url(x)
	}, "", USE.NAMES = FALSE)

	noCustomSet <- !grepl("id", realProfiles)
	realProfiles[noCustomSet] <- sid[!useDictSet][noCustomSet]

	realProfiles <- gsub("http://steamcommunity.com/id/", "", realProfiles)
	convName[!useDictSet] <- gsub("[/]", "", realProfiles)

	saveSet <- !useDictSet
	saveSet[!useDictSet][noCustomSet] <- FALSE

	if(saveArchive && any(saveSet)){

		addMat <- cbind(convName, sid)[saveSet, , drop = FALSE]
		write.table(
			addMat,
			file = system.file("data", "playerDict.csv", package = "tf2statr"),
			append = TRUE, row.names = FALSE, col.names = FALSE, sep = ";")

	}

	return(convName)

}

getArchiveSIDfromName <- function(iden){

	data("playerDict", package = "tf2statr", envir = environment())
	playerDict <- as.matrix(playerDict)

	convSids <- rep(NA, length(iden))

	dictNums <- match(iden, playerDict[, 1])
	convSids[dictNums] <- playerDict[dictNums, 2]


	badSet <- is.na(convSids)
	if(any(badSet)){
		warning(paste("These players are not in the archive dictionary:",
			paste(iden[badSet], sep = ','),
			"Update the archive by doing it manually, or using a log that they played in",
			sep = "\n"))
	}


	return(convSids)
}


easyScrape <- function(url, xpathCap, failRegex){
	htmlFile <- xml2::read_html(url)
	if( grepl(failRegex, htmlFile) ){
		stop(paste0("'", url, "'", " is not a real web page/search (404)"))
	}
	node <- rvest::html_node(htmlFile, xpath = xpathCap)
	return(node)
}

`addSeasonQs<-` <- function(qs, value){
	#Go through league uploads
}

notUnique <- function(vec){
	dupSet <- duplicated(vec) | duplicated(vec, fromLast = TRUE)
	if(any(dupSet)){
		return(unique(vec[dupSet]))
	} else {
		return(vec)
	}
}
