user_input_bad_code = list(
	list(code = "ieland", values = c(5100, 5128)),
	list(code = "køn", values = c(1,2)),
	list(code = "tid", values = NA),
	list(code = "not_in_table", values = c(2,3)))

user_input_bad_values = list(
	list(code = "køn", values = c(1,2)),
	list(code = "ieland", values = c(17, 5100, 5128)), # 17 is not a valid value
	list(code = "tid", values = NA))

user_input = list(list(code = "ieland", values = c(5100, 5128)),
									list(code = "køn", values = c(1,2)),
									list(code = "tid", values = NA))

test_that("error behavior with wrong and correct user input", {
  expect_error(check_variables_code("folk1c", user_input = user_input_bad_code))
	expect_true(check_variables_code("folk1c", user_input = user_input_bad_values)) # doesn't detect bad values, only bad codes
	expect_true(check_variables_code("folk1c", user_input = user_input))
	expect_true(check_variables_values(get_valid_variable_values("folk1c", "alder"), c("0-4", "5-9", "20-24")))
	expect_false(check_variables_values(get_valid_variable_values("folk1c", "alder"), c("0-4", "5-9", "20-24", "100+"))) # 100+ is not a valid value
})

test_that("error with wrong variable code", {
	expect_error(get_data(table_id = "folk1c", variables = user_input_bad_code))
})

test_that("warning with wrong variable values", {
	expect_warning(get_data(table_id = "folk1c", variables = user_input_bad_values))
})

test_that("correct column names and output class", {
	expect_equal(tolower(colnames(get_data(table_id = "folk1c", variables = user_input))), c("ieland", "køn", "tid", "indhold"))
	expect_is(get_data(table_id = "folk1c", variables = user_input), "data.frame")
})
