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

#'Scrap comp.tf event pages for logs IDs
#'
#'This goes to a given comp.tf website and grabs logs for the first bracket
#'listed or the <insert season entry>. This links to the 'data/eventArchive'
#'JSON archive so that comp.tf is not queried multiple times. However, sometimes
#'the log IDs need to be updated in the situation with an ongoing event. If the
#'event is in the archive it will ask the user if they want to update the entry
#'which would do the whole process like it was a new entry. With a new entry, by
#'default, this archive is updated with a new entry. The difference between the
#'archive and the ids are that the JSON archive returns a list with the names of
#'the tournament while the function returns a named character vector for ease of
#'throwing into 'getLog' easy transfer between the two can be done with
#'\code{as.list()} or \code{c( , recursive = TRUE)}
#'
#'@param comptfToken The name of the comp.tf page in the url
#'@param scrapeLoc Is this page a tourney (scrape a bracket) or a season (scrape
#'  a table)
#'@param withNames Should the identifier of each of the logs be added as a names
#'  for output?
#'@param saveArchive Should the new query be saved in the tf2statr library in
#'  'data'?
#'@param shReDownload If the event is in the the archive, should it redownload
#'  the file?
#'
#'@return A character vector with all the logs. A named character vector if
#'  "withNames" is TRUE
#'@export
getLogIDsComptf <- function(
	comptfToken,
	scrapeLoc = c("Tourney", "Season")[1],
	withNames = TRUE,
	saveArchive = TRUE,
	shReDownload = NULL){

	data("eventArchive", package = "tf2statr", envir = environment())

	if(any(grepl(comptfToken, names(eventArchive)))){
		if(is.null(shReDownload)){
			resp <- readline(
				"This event is already in the archive. Want to update? y or n: ")
			shReDownload <- grepl("y", resp, ignore.case = TRUE)
		}

		if(!shReDownload){
			return(c(eventArchive[[comptfToken]], recursive = TRUE))
		}
	}

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

	if(saveArchive){
		eventArchive[[comptfToken]] <- as.list(ids)
		writeLines(jsonlite::toJSON(eventArchive),
			con = system.file("data", "eventArchive", package = "tf2statr"))
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
	logLabels <- gsub("\\s", "_", logLabels)
	logLabels <- logLabels[nzchar(logLabels)]

	return(logLabels)

}
