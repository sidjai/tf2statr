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

#' Scrap comp.tf event pages for logs IDs
#'
#' @param comptfToken The name of the comp.tf page in the url
#' @param scrapeLoc Is this page a tourney (scrape a bracket) or a season
#'   (scrape a table)
#' @param withNames Should the identifier of each of the logs be added as a
#'   names for output?
#'
#' @return A character vector with all the logs. A named character vector if
#'   "withNames" is TRUE
#' @export
getLogIDsComptf <- function(
	comptfToken,
	scrapeLoc = c("Tourney", "Season")[1],
	withNames = TRUE){
  pageUrl <- paste0("http://comp.tf/wiki/", comptfToken)

  page <- xml2::read_html(pageUrl)

  if(any(grepl("The page you are looking for cannot be found", page))){
    stop(paste(
      "The provided web page:",
      pageUrl,
      "is not a valid comp.tf web page"))
  }

  bracketXp <- paste("//*[@id='mw-content-text']",
    "div[contains(@class, 'bracket-wrapper')][1]/div[1]", sep = '/')

  innerXp <- "div[@class = 'bracket-column' and not(contains(@style,'width:10px'))]"
  allGamesXp <- paste(paste0("div[1]/", innerXp), innerXp, sep = " | ")

  brNodes <- rvest::html_nodes(page, xpath = bracketXp)
  nodes <- rvest::html_nodes(brNodes, xpath = allGamesXp)

  rawTitles <- rvest::html_text(rvest::html_node(nodes, xpath = "div[1]/div[1]"))

  spaceSet <- grepl("^\\s+", rawTitles)
  nodes <- nodes[!spaceSet]

  ids <- parseBracketColumns(nodes)



	if(withNames){
		enuNames <- getMatchNames(nodes)
		if(length(enuNames) != length(ids)){
			stop("Naming logs in comp.tf bracket extract screwed up")
		}
		names(ids) <- enuNames
	}

  return(ids)
}

parseBracketColumns <- function(colNodes){
  logXp <- paste0("div[not(position()=1)]/div[2]/div[last()]/div[last()]/",
		"div[@class='map']/a[contains(@href,'logs.tf')]")

  ids <- rvest::html_attr(
  	rvest::html_nodes(colNodes, xpath = logXp),
  	"href")

  ids <- gsub("http://logs.tf/", "", ids)
  return(ids)
}

getMatchNames <- function(nodes){

	baseLabel <- rvest::html_text(rvest::html_node(nodes, xpath = "div[1]/div[1]"))

	logLabels <- mapply(function(node, nam){
		matchNs <- rvest::html_nodes(node, xpath = "div[not(position()=1)]")
		out <- paste0(rep(nam, length(matchNs)), " match", 1:length(matchNs))

		out <- mapply(function(node, nam){
			endXp <- paste0("div[2]/div[last()]/div[last()]/",
				"div[@class='map']/a[contains(@href,'logs.tf')]")
			mapNs <- rvest::html_nodes(node, xpath = endXp)
			if(length(mapNs) > 0){
				return(paste0(rep(nam, length(mapNs)), " map", 1:length(mapNs)))
			} else {
				return("")
			}

		}, matchNs, out)

	},nodes, baseLabel)

	logLabels <- c(logLabels, recursive = TRUE)
	logLabels <- logLabels[nzchar(logLabels)]

	return(logLabels)

}
