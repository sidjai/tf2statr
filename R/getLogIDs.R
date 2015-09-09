#' Use logs.tf's query JSON API
#'
#' The log.tf query api has three queries that can be made, an uploader, a name
#' and a player. Out of these, the player is the most reliable without prior knowledge.
#' The result is a vector of log IDs that can be then feed into 'getLog'
#' One of the queries are required but all three can be supplied
#'
#' @param title The title of the match that you want to query
#' @param uploader The person or bot that actually put the game on log.tf
#' @param player The SteamID3 [U:int:numbers] or the 16 digit id
#' @param num The maximum number of IDs to output
#'
#' @return a vector of IDs
#' @export
getLogIDsJSON <- function(title = "", uploader = "", player = "", num = 10){
  tokens <- c()
  if(nzchar(title)){
    tokens <- paste0("title=", title)
  }
  if(nzchar(uploader)){
    tokens <- paste0("uploader=", uploader)
  }
  if(nzchar(player)){
    tokens <- paste0("player=", player)
  }

  if(length(tokens) == 0){
    stop("Please supply a search parameter of either a title or an uploader")
  }
  tokens <- c(tokens, paste0("limit=", num))
  jsonSearch <- paste0("http://logs.tf/json_search?", paste(tokens, collapse = "&"))
  return(parseJSONSearch(jsonSearch, num))


}

parseJSONSearch <- function(searchUrl, reqNum){
  query <- jsonlite::fromJSON(searchUrl)
  if(!query$success){
    stop(paste(
      "The search:",
      searchUrl,
      "had no hits with response:",
      query[[1]]))
  }

  if(query$results == reqNum){
    warning(sprintf(
      "More results may be online since you requested %d matches and got %d matches",
      reqNum,
      query$results))
  }

  return(query$logs$id)
}
