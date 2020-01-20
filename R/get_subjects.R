#' Get a list of subjects covered in the data bank
#'
#' @param language Language for the return object. Default = \code{"en"}
#' @param recursive Whether subtopics/tables will be retrieved all the way down the hierarchy. Otherwise, only the closest level under the provided subjects will be retrieved. Default = \code{FALSE}
#' @param include_tables Whether the result should contain tables. Otherwise, only subjects are returned. Default = \code{FALSE}
#' @param subjects Provide specific subject id's to get subtopics. E.g. \code{subjects = c("02", "2419")}
#'
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#'
#' @return A data frame
#' @export

get_subjects <- function(language = c("en", "da"), recursive = FALSE, include_tables = FALSE, subjects = NULL){

	# evaluate language choices
	language <- match.arg(language)
	# Treat objects "as is"
	if (!is.null(subjects)) subjects <- I(subjects)

	call_body <- list(lang = language,
										recursive = recursive,
										includeTables = include_tables,
										subjects = subjects)

	result <- httr::POST(SUBJECTS_ENDPOINT, body = call_body, encode = "json")

	check_http_type(result)

	return(jsonlite::fromJSON(httr::content(result)))
}
