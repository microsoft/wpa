test_that("create_boxplot produces ggplot object", {
  result <- sq_data %>%
    create_boxplot(hrvar = "Organization", metric = "Email_hours")
  expect_s3_class(result, "ggplot")
})

test_that("create_boxplot handles different return types", {
  # Test table return
  result_table <- sq_data %>%
    create_boxplot(hrvar = "Organization", metric = "Email_hours", return = "table")
  expect_s3_class(result_table, "data.frame")
  
  # Test plot return
  result_plot <- sq_data %>%
    create_boxplot(hrvar = "Organization", metric = "Email_hours", return = "plot")
  expect_s3_class(result_plot, "ggplot")
})

test_that("create_boxplot error handling", {
  expect_error(
    sq_data %>% create_boxplot(hrvar = "InvalidVar", metric = "Email_hours")
  )
})
