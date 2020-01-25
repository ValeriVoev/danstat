#' Get a list of subjects covered in the data bank
#'
#' @param subjects Provide specific subject id's to get subtopics. E.g. \code{subjects = c("02", "2419")}
#' @param recursive Whether subtopics/tables will be retrieved all the way down the hierarchy. Otherwise, only the closest level under the provided subjects will be retrieved. Default = \code{FALSE}
#' @param include_tables Whether the result should contain tables. Otherwise, only subjects are returned. Default = \code{FALSE}
#' @param language Language for the return object. Default = \code{"en"}
#'
#'
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#'
#' @return A data frame
#' @export
#' @examples
#' # Get all subjects
#' all_subjects <- get_subjects()
#'
#' # Or get (sub)subjects for specific subjects
#' some_subjects <- get_subjects(subjects = c("02", "2419"))
#'
#' # Get all subject hierarchy for a given subject
#' subject_with_hierarchy <- get_subjects(subjects = "02", recursive = TRUE)


get_subjects <- function(subjects = NULL, recursive = FALSE, include_tables = FALSE, language = c("en", "da")){

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

