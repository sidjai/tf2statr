getLogIDs <- function(title = "", uploader = "", player = "", num = 10){
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
