getLog <- function(
  logId,
  altNames = c(),
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


  cleanUpVec <- plyMat[, "dmg_real"] / plyMat[, "dmg"]
  dmgPerVec <- plyMat[, "dmg"] / plyMat[, "hr"]
  dmgPerVec[is.infinite(dmgPerVec)] <- NA #Medics don't get heals

  totHeal <- rep(0,2)
  medSet <- names(match$healspread)
  totHeal[plyMat[medSet, "team"]] <- plyMat[medSet, "heal"]
  percentVec <- plyMat[, "hr"] / totHeal[plyMat[, "team"]]



  finNames <- c(colnames(plyMat), "daphr", "hr_ratio", "dmg_realpdmg")
  plyMat <- cbind(plyMat, dmgPerVec, percentVec, cleanUpVec)
  colnames(plyMat) <- finNames

  if(length(altNames) == dim(plyMat)[2]){
  	rownames(plyMat) <- altNames
  } else if(length(altNames) > 1){
  	stop(paste(
  		"Please provide Alternative names for all the players, you provided",
  		altNames, sep = "\n"))
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
  simpleSet <- vapply(lplayer, function(x){ !is.list(x) && !is.data.frame(x) }, TRUE)
  cropped <- as.numeric(lplayer[simpleSet])
  names(cropped) <- names(lplayer[simpleSet])
  return(cropped)
}

aggregateStats <- function(lmatch, statFun = mean){

  allPlayerNames <- vapply(lmatch, function(x){
    names(x$players)
  }, rep("e", length(lmatch[[1]]$players)))
  allPlayerNames <- unique(c(allPlayerNames, recursive = TRUE))

  agg <- vector(mode = "list", length = length(allPlayerNames))
  names(agg) <- allPlayerNames

  for(ply in allPlayerNames){
    allStatMat <- vapply(lmatch, function(log){
      c(mat$players$ply, recursive = TRUE)
    }, length(allstats))

    agg$ply <- apply(allStatMat, statFun, na.rm = TRUE)

  }

  return(agg)
}
