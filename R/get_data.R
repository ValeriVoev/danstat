#' Get data for a particular table and variable selection
#'
#' @param table_id Table identifier, e.g. "folk1a"
#' @param variables A list with variable code-values pairs. Each code-values pair should be a named list with names "code" and "values".
#' @param language Language for the return object. Default = \code{"en"}
#'
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#'
#' @return A data frame
#' @export

get_data <- function(table_id, variables, language = c("en", "da")){

	# evaluate language choices
	language <- match.arg(language)

	variables <- make_variable_input(table_id, variables)

	call_body <- list(table = table_id,
										lang = language,
										format = "CSV",
										delimiter = "Tab",
										variables = variables)

	result <- httr::POST(DATA_ENDPOINT, body = call_body, encode = "json")

	check_http_type(result, expected_type = "text/csv")

	return(httr::content(result, type = "text/tab-separated-values", encoding = "UTF-8"))
}


check_variables_code <- function(table_id, user_input){

	# rertieve variables for this table
	valid_vars <- get_table_metadata(table_id = table_id, variables_only = TRUE)

	# check variable names (ids)
	user_vars <- sapply(user_input, function(x) x[[1]])

	if (!all(tolower(user_vars) %in% tolower(valid_vars$id))) {
		bad_input_index <- which( !(tolower(user_vars) %in% tolower(valid_vars$id) ))
		stop(paste0("variable code '", user_vars[bad_input_index], "' is not a variable in table '", table_id,
									 "'. Run get_table_metadata('", table_id, "', variables_only = TRUE) for valid variable codes."),
						call. = FALSE)

	}

  return(TRUE)

}

check_variables_values <- function(valid_values, user_values){

	if (any(!(user_values %in% valid_values))) return(FALSE)

	return(TRUE)

}

make_variable_input <- function(table_id, user_input) {

	# Check variable codes
	check_variables_code(table_id, user_input)

	output_variable_list <- list()

	ind <- 1
	for (variable_pair in user_input) {
		# check column names of variables
		if (any(!(names(variable_pair) == c("code", "values")))) stop("variables code-values pairs need to be a named lists with names 'code' and 'values'", call. = FALSE)

		variable_id <- variable_pair$code
		variable_values <- variable_pair$values
		if (any(is.na(variable_values))) variable_values <- "*"

		valid_values <- c(get_valid_variable_values(table_id, variable_id), "*")

		if (check_variables_values(valid_values, variable_values)) {

			output_variable_list[[ind]] <- list(code = variable_id, values = I(variable_values))

		} else {

			warning(paste0("Values for ", variable_id, " are not valid... skipping ", variable_id), call. = FALSE)
			next

		}
		ind <- ind + 1
	}

	return(output_variable_list)
}
