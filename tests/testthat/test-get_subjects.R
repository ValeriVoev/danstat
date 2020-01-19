test_that("get_subjects no inputs works", {
	expect_is(get_subjects(), "data.frame")
	expect_equal(colnames(get_subjects()), c("id", "description", "active", "hasSubjects", "subjects"))
})

test_that("get_subjects recursive works", {
	with_recursion <- get_subjects(recursive = TRUE)$subjects[[1]]
	without_recursion <- get_subjects()$subjects[[1]]
	expect_true(length(with_recursion) > 0)
	expect_true(length(without_recursion) == 0)
})
