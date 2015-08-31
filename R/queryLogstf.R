queryLogstf <- function(
  players = c(),
  teamName = "",
  season = c(),
  tournament = ""){

  queries <- matrix(nrow = 1, ncol = 1)

  if(length(players) > 0){
    addPlayerQs(queries) <- players
  }

  if(length(season) > 0){
    addSeasonQs(queries) <- season
  }

  if(length(tournament) > 0){
    addTourneyQs(queries) <- season
  }

  if(dim(queries)[1] == 1){
    stop(paste("Please supply a list of",
    "players, the team name or the tournament to query the logs"))
  } else { queries <- queries[-1,] }

  ids <- getLogIDs(
    title = queries$title,
    uploader = queries$uploader,
    player = queries$player,
    num = 10)

  andIds <- notUnique(ids)

  llogs <- lapply(andIds, getLog, altNames = players)

  return(llogs)

}

`addPlayerQs<-` <- function(qs, value){
  #from nickname get steam name

  return(qs + players)
}

`addSeasonQs<-` <- function(qs, value){
  #Go through league uploads
}

`addTourneyQs<-` <- function(qs, value){
  #parse brackets
}
