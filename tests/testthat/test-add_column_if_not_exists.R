test_that("adds column with NA default when column is absent", {
  df <- tibble::tibble(a = 1:3)
  result <- add_column_if_not_exists(df, "flag")
  expect_true("flag" %in% names(result))
  expect_true(all(is.na(result$flag)))
})

test_that("adds column with custom default value when column is absent", {
  df <- tibble::tibble(a = 1:3)
  result <- add_column_if_not_exists(df, "flag", default_value = "test_value")
  expect_equal(result$flag, rep("test_value", 3))
})

test_that("does not modify existing column", {
  df <- tibble::tibble(a = 1:3, flag = c("x", "y", "z"))
  result <- add_column_if_not_exists(df, "flag", default_value = "")
  expect_equal(result$flag, c("x", "y", "z"))
})
