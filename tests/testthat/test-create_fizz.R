test_that("create_fizz produces ggplot object", {
  result <- sq_data %>%
    create_fizz(hrvar = "Organization", metric = "Email_hours")
  expect_s3_class(result, "ggplot")
})

test_that("create_fizz handles different return types", {
  # Test table return
  result_table <- sq_data %>%
    create_fizz(hrvar = "Organization", metric = "Email_hours", return = "table")
  expect_s3_class(result_table, "data.frame")
})
