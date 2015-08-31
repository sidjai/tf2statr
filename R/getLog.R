getLog <- function(
  logId,
  altNames = c(),
  keepClassSpec = FALSE,
  keepChat = FALSE){


  matchUrl <- paste0("http://logs.tf/json/", logId)

  niceMatch <- match <- fromJSON(matchUrl)

  killInd <- grep("version", names(match))
  if(!keepChat) killInd <- c(killInd, grep("chat", names(match)))
  if(!keepClassSpec) killInd <- c(killInd, grep("class", names(match)))

  niceMatch <- niceMatch[-killInd]


  return(niceMatch)


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
