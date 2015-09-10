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
    llogs <- lapply(andIds, getLog, altNames = players)
    return(llogs)
  } else {
    return(ids)
  }

}

`addPlayerQs<-` <- function(qs, value){
  #from tf.tv name get steamID3
  tftvurl <- paste0("http://www.teamfortress.tv/user/", value)
  xpathIDs <- '//*[@id="content-inner"]/div[1]/table[1]'
  htmlFile <- xml2::read_html(tftvurl)
  if( grepl("Page Not Found", htmlFile) ){
  	stop(paste0("'", value, "'", " is not a real tf.tv username"))
  }
  node <- rvest::html_node(htmlFile, xpath = xpathIDs)
  tftable <- rvest::html_table(node)
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
