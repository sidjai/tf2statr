queryLogstf <- function(
  players = c(),
  teamName = "",
  season = c(),
  tournament = ""){

  queries <- matrix(nrow = 1, ncol = 3)
  colnames(queries) <- c("title", "uploader", "player")

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
    title = queries[, "title"],
    uploader = queries[, "uploader"],
    player = queries[, "player"],
    num = 10)

  andIds <- notUnique(ids)

  llogs <- lapply(andIds, getLog, altNames = players)

  return(llogs)

}

#'import(rvest)
`addPlayerQs<-` <- function(qs, value){
  #from tf.tv name get steamID3
  tftvurl <- paste0("http://www.teamfortress.tv/user/", value)
  xpathIDs <- '//*[@id="content-inner"]/div[1]/table[1]'
  node <- html_node(read_html(tftvurl), xpath = xpathIDs)
  tftable <- html_table(node)
  sid3 <- tftable[tftable[,1] == "SteamID3", 2]

  return(rbind(qs, c("", "", sid3)))
}

`addSeasonQs<-` <- function(qs, value){
  #Go through league uploads
}

`addTourneyQs<-` <- function(qs, value){
  #parse brackets
}

notUnique <- function(vec){
  dupSet <- duplicated(vec) | duplicated(vec, fromLast = TRUE)
  return(unique(vec[dupSet]))
}
