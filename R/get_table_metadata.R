#' Title
#'
#' @param table_id Table identifier, e.g. "folk1a"
#' @param language Language for the return object. Default = \code{"en"}
#' @param variables_only If \code{TRUE} returns only information about the variables in the table
#'
#' @return A list with information about the table, like documentation url, variable description, etc. If \code{variables_only = TRUE}, returns a data frame with variable information.
#' @export


get_table_metadata <- function(table_id, language = c("en", "da"), variables_only = FALSE){

	# evaluate language choices
	language <- match.arg(language)

	call_body <- list(lang = language,
										table = table_id)

	result <- httr::POST(METADATA_ENDPOINT, body = call_body, encode = "json")

	check_http_type(result)

	full_result <- jsonlite::fromJSON(httr::content(result))

	if (variables_only) return(full_result$variables)

	return(full_result)

}

get_valid_variable_values <- function(table_id, variable_id){

	vars <- get_table_metadata(table_id = table_id, variables_only = TRUE)
	return(vars[["values"]][[which(tolower(vars$id) == tolower(variable_id))]]$id)

}
