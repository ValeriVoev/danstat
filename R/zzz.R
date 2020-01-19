SUBJECTS_ENDPOINT <- "https://api.statbank.dk/v1/subjects"
TABLES_ENDPOINT <- "https://api.statbank.dk/v1/tables"
METADATA_ENDPOINT <- "https://api.statbank.dk/v1/tableinfo"
DATA_ENDPOINT <- "https://api.statbank.dk/v1/data"

check_http_type <- function(response, expected_type = "text/json") {
	if (httr::http_type(response) != expected_type) {
		stop(paste("API did not return", expected_type), call. = FALSE)
	}
}
