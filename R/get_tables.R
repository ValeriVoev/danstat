#' Get a list of stables in the data bank
#'
#' @param subjects Provide specific subject id's to get subtopics. E.g. \code{subjects = c("02", "2419")}. Can be retrieved with \code{get_subjects()}
#' @param pastdays Return only tables which have been updated within this number of days
#' @param include_inactive Whether to return tables that are no longer updated
#' @param language Language for the return object. Default = \code{"en"}
#'
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#'
#' @return A data frame
#' @export
#' @examples
#' # Get all tables
#' all_tables <- get_tables()
#'
#' # Or get tables for specific subjects
#' some_tables <- get_tables(subjects = c("2", "3413"))
#'
#' # Get all tables updated within the past 3 days
#' tables_past3days <- get_tables(pastdays = 3)


# TODO: Consider implementing a search_term or regex to do a lookup in the "text" column which is a kind of description
get_tables <- function(subjects = NULL, pastdays = NA_integer_, include_inactive = FALSE, language = c("en", "da")){

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
