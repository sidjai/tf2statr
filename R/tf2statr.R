#' tf2statr: Managing and querying Team Fortress 2 stats
#'
#' This package provides tools to query logs.tf for stats and attempts to
#' provide advanced queries and cleaning functions using key TF2 sites.
#'
#' @docType package
#' @name tf2star
NULL

#' Archive of player names and their corresponding Steam ID3
#'
#' A dictionary for the program to provide alternative names for the logs
#' instead of the steam ID3 which is unknown. Names have to be relatively
#' constant so results can be compared across games and timeperiods.
#'
#' @format A data frame, "playerDict" with 2 columns and an ever expanding
#'   number of rows: \describe{ \item{name}{default is the custom profile vanity
#'   name as a string} \item{carat}{The Steam ID3 of the player in the form of
#'   [u:int:numbers] as a character} }
#' @source the redirect from the Steam ID3 from Valve. The function that creates
#'   more rows using this method is \code{convSID2name}
#' @name playerDict
#' @docType data
NULL

#' Archive of tf2 events and their corresponding log ID
#'
#' A dictionary for the comp.tf website so you don't have to re scrape it
#' everytime you want to examine a data set. The raw data is in JSON format
#' while the '.R' code translates it into a list of lists using 'jsonlite'
#' package.
#'
#' @format A list of the different events in variable "events" \describe{
#'   \item{nameOfFirstLevel}{The comp.tf webpage name last portion}
#'   \item{nameOfSecondLevel}{The different games in the event with designations
#'   in the naming scheme. For tournaments, it is sequenced as the following:the
#'   name of the round as given by comp.tf, the match number from the top of the
#'   bracket and then the map number in that match.} \item{secondLevel}{The log
#'   ID is a string of the log.tf log ID} }
#' @source \url{http://comp.tf/wiki/Main_Page} with the function that scrapes it
#'   and saves new entires the archive \code{\link{getLogIDsComptf}}
#' @name eventArchive
#' @docType data
NULL

#' Insomnia 55 parsed logs
#'
#' The final output for the i55 event.
#'
#' @format A list of all the events in the Insomnia 55 tournament
#' \describe{
#'   \item{nameOfFirstLevel}{
#'   The different games in the event with designations in the naming scheme. It
#'   is sequenced as the following: the name of the round as given by comp.tf,
#'   the match number from the top of the bracket and then the map number in
#'   that match.}
#'   \item{logsVar: player}{
#'   The class dependent stats for each player that are not}
#'   \item{logsVar: table}{The single number statistics for each player}
#'	 \item{logs$table col: k}{The single number statistics for each player}
#'	 \item{logs$table col: team}{Blue: 1, Red: 2}
#'	 \item{logs$table col: kills}{Number of kills}
#'	 \item{logs$table col: deaths}{Number of deaths}
#'	 \item{logs$table col: assists}{Number of assists}
#'	 \item{logs$table col: suicides}{
#'	 Number of times got unassisted kill be "theWorld" or typed "kill" in
#'	 console}
#'	 \item{logs$table col: kapd}{(Kills + assists) / deaths}
#'	 \item{logs$table col: kpd}{kills / death}
#'	 \item{logs$table col: dmg}{
#'	 Total amount of damage done irrespective of inflicted damage}
#'	 \item{logs$table col: dmg_real}{
#'	 Amount of damage actually inflicted on a player which is adjusted for heals
#'	 and overdamage}
#'	 \item{logs$table col: dt}{Total Damage taken by the player}
#'	 \item{logs$table col: dt_real}{
#'	 Permenant Damage taken by the player adjusting for heals}
#'	 \item{logs$table col: hr}{Amount of heals taken by the player}
#'	 \item{logs$table col: lks}{}
#'	 \item{logs$table col: as}{Airshots}
#'	 \item{logs$table col: dapd}{Damage / deaths}
#'	 \item{logs$table col: dapm}{Damage / minute}
#'	 \item{logs$table col: ubers}{Number of ubers given}
#'	 \item{logs$table col: drops}{
#'	 Number of times a full uber was dropped due to a death}
#'	 \item{logs$table col: medkits}{Amount of medkits picked up by the player}
#'	 \item{logs$table col: medkits_hp}{Healing provided by medkits picked up}
#'	 \item{logs$table col: backstabs}{Number of backstabs given as a spy}
#'	 \item{logs$table col: headshots}{Number of headshots}
#'	 \item{logs$table col: headshots_hit}{Number of headshot deaths}
#'	 \item{logs$table col: sentries}{Number of sentires built}
#'	 \item{logs$table col: heal}{Amount of healing given as a medic}
#'	 \item{logs$table col: cpc}{Number of captures}
#'	 \item{logs$table col: ic}{}
#'	 \item{logs$table col: daphr}{Damage / Heals recieved}
#'	 \item{logs$table col: hr_ratio}{
#'	 Heals recieved / total heals given by the medic}
#'	 \item{logs$table col: dmg_realpdmg}{
#'	 Actual damage given / Total damage given}
#'	 \item{logs$table col: num_streaks}{
#'	 Number of killstreaks of length 3 kills or more}
#' }
#' @source \url{http://logs.tf/} with the function makes the list
#'   \code{\link{queryLogstf}} with arguments, (tournament = "Insomnia55")
#' @name i55
#' @docType data
NULL
