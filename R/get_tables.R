#' Get a list of stables in the data bank
#'
#' @param language Language for the return object. Default = \code{"en"}
#' @param subjects Provide specific subject id's to get subtopics. E.g. \code{subjects = c("02", "2419")}. Can be retrieved with \code{get_subjects()}
#' @param include_inactive Whether to return tables that are no longer updated
#' @param pastdays Return only tables which have been updated within this number of days
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#'
#' @return A data frame
#' @export


# TODO: Consider implementing a search_term or regex to do a lookup in the "text" column which is a kind of description
get_tables <- function(language = c("en", "da"), subjects = NULL, pastdays = NA_integer_, include_inactive = FALSE){

	# evaluate language choices
	language <- match.arg(language)

	stopifnot(is.numeric(pastdays))

	# Treat objects "as is"
	if (!is.null(subjects)) subjects <- I(subjects)

	call_body <- list(lang = language,
										subjects = subjects,
										pastdays = pastdays,
										includeinactive = include_inactive)

	result <- httr::POST(TABLES_ENDPOINT, body = call_body, encode = "json")

	check_http_type(result)

	return(jsonlite::fromJSON(httr::content(result)))
}