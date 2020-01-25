#' Title
#'
#' @param table_id Table identifier, e.g. "folk1a"
#' @param variables_only If \code{TRUE} returns only information about the variables in the table
#' @param language Language for the return object. Default = \code{"en"}
#'
#' @return A list with information about the table, like documentation url, variable description, etc. If \code{variables_only = TRUE}, returns a data frame with variable information.
#' @export
#' @examples
#' # Get table metadata for a given table
#' table_meta <- get_table_metadata(table_id = "folk1c") # a list
#'
#' # Get only information about the variables in the table
#' table_meta_vars <- get_table_metadata(table_id = "folk1c", variables_only = TRUE) # a data frame


get_table_metadata <- function(table_id, variables_only = FALSE, language = c("en", "da")){

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
