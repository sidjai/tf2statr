#' Grabs the raw JSON of the log and clean it up
#'
#' @param logId The sequential ID that logs.tf uses for a given game
#' @param useAltNames should the program use the alternative names from the
#'	 player dictionary in 'data/playerDict.csv' or go find the alt name using
#'	 their steamID and their custom profile name?
#' @param keepClassSpec Do you want to keep the class specific stats for
#'	 everyone?
#' @param keepChat Do you want to keep the chat from the game?
#'
#' @return A list with the logs, the player specific stats in $player and the
#'	 table version of the numeric stats in $table with some added stats
#' @export
getLog <- function(
	logId,
	useAltNames = FALSE,
	keepClassSpec = FALSE,
	keepChat = FALSE){


	matchUrl <- paste0("http://logs.tf/json/", logId)

	niceMatch <- match <- jsonlite::fromJSON(matchUrl)

	killInd <- grep("version", names(match))
	if(!keepChat) killInd <- c(killInd, grep("chat", names(match)))
	if(!keepClassSpec) killInd <- c(killInd, grep("class", names(match)))

	niceMatch <- niceMatch[-killInd]



	plyMat <- t(vapply(niceMatch$players,function(x){
		cleanLogMat(x, teamNames = names(match$teams))
	},rep(1.1, 27)))

	if(!verifyLog(niceMatch)){
		plyMat[,] <- NA
		dmgPerVec <- rep(NA, dim(plyMat)[1])
		percentVec <-	cleanUpVec <- streakVec <- dmgPerVec

	} else {
		cleanUpVec <- plyMat[, "dmg_real"] / plyMat[, "dmg"]
		dmgPerVec <- plyMat[, "dmg"] / plyMat[, "hr"]
		dmgPerVec[is.infinite(dmgPerVec)] <- NA #Medics don't get heals

		totHeal <- rep(0,2)
		medSet <- names(match$healspread)
		totHeal[plyMat[medSet, "team"]] <- plyMat[medSet, "heal"]
		percentVec <- plyMat[, "hr"] / totHeal[plyMat[, "team"]]

		numStreaks <- c(table(match$killstreaks$steamid))
		streakVec <- rep(0, dim(plyMat)[1])
		streakVec[match(names(numStreaks), rownames(plyMat))] <- numStreaks

	}

	finNames <- c(colnames(plyMat),
		"daphr", "hr_ratio", "dmg_realpdmg", "num_streaks")
	plyMat <- cbind(plyMat, dmgPerVec, percentVec, cleanUpVec, streakVec)
	colnames(plyMat) <- finNames

	if(useAltNames){
		rownames(plyMat) <- convSID2name(rownames(plyMat))
	}

	niceMatch$table <- plyMat
	for(pind in 1:length(niceMatch$players)){
		dupSet <- !is.na(match(names(niceMatch$players[[pind]]), colnames(plyMat)))
		niceMatch$players[[pind]][dupSet] <- NULL
	}

	return(niceMatch)


}

cleanLogMat <- function(lplayer, teamNames){
	lplayer$team <- match(lplayer$team, teamNames)
	simpleSet <- vapply(lplayer, function(x){
		!is.list(x) && !is.data.frame(x)
	}, TRUE)
	cropped <- as.numeric(lplayer[simpleSet])
	names(cropped) <- names(lplayer[simpleSet])
	return(cropped)
}

# Check if log is incomplete / corrupted
verifyLog <- function(log){

	numFromMed <- (length(c(log$healspread, recursive = TRUE))
		+ length(log$healspread))
	numFromPly <- length(log$players)
	goodMedStat <- (numFromPly == numFromMed)

	return(goodMedStat)
}

#' Do statistics on multiple logs
#'
#' @param lmatch a list of logs produced by \code{\link{getLog}}
#' @param statFun The statistical function that you want to use on the logs.
#'	 Default is a simple mean, but can include any function, R or user made,
#'	 with a \code{na.rm} parameter
#'
#' @return A matrix of the model appliled log table for all the participants
#'	 involved in any of the games. There is an extra column of \code{gp}, or
#'	 games played.
#' @export
aggregateStats <- function(lmatch, statFun = mean){

	lPlayers <- lapply(lmatch, function(x){
		rownames(x$table)
	})
	allPlayerNames <- unique(c(lPlayers, recursive = TRUE))

	gamesPlayed <- vapply(allPlayerNames, function(nam){
		reg <- gsub("\\[|\\]", "", nam)
		didParticipate <- vapply(lPlayers, function(logPlys){
			any(grepl(reg, logPlys))
		}, TRUE)
		sum(didParticipate)
	}, 1)

	agg <- array(NA, dim=c(
		length(allPlayerNames),
		dim(lmatch[[1]]$table)[2],
		max(gamesPlayed)))



	dimnames(agg)[[1]] <- allPlayerNames
	dimnames(agg)[[2]] <- colnames(lmatch[[1]]$table)

	gpcnt <- rep(1, length(allPlayerNames))
	for(log in lmatch){
		aggInds <- match(rownames(log$table), allPlayerNames)
		agg[aggInds, , gpcnt[aggInds]] <- log$table

		if(!all(is.na(log$table))){
			gpcnt[aggInds] <- gpcnt[aggInds] + 1
		}

	}

	agg <- apply(X = agg, MARGIN = c(1, 2), FUN = statFun, na.rm = TRUE)

	agg <- cbind(agg, gp = gamesPlayed)

	return(agg)
}
