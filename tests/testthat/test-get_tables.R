test_that("subject 'as is' I() works", {
  expect_identical(get_tables(subjects = "3465"), get_tables(subjects = c("3465")))
})

test_that("numeric pastdays", {
	expect_error(get_tables(pastdays = "a"))
})
