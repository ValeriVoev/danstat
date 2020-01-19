test_that("returns an error without a table_id", {
	expect_error(get_table_metadata())
})

test_that("returns the right object class", {
	expect_is(get_table_metadata(table_id = "folk1a"), "list")
	expect_is(get_table_metadata(table_id = "folk1a", variables_only = TRUE), "data.frame")
})


test_that("get_valid_variable_names returns correct values", {
	expect_equal(get_valid_variable_values("folk1c", "køn"), c("TOT", "1", "2"))
})
