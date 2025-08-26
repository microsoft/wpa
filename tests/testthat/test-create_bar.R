test_that("create_bar produces ggplot object", {
  result <- sq_data %>%
    create_bar(hrvar = "Organization", metric = "Email_hours")
  expect_s3_class(result, "ggplot")
})

test_that("create_bar handles different return types", {
  # Test table return
  result_table <- sq_data %>%
    create_bar(hrvar = "Organization", metric = "Email_hours", return = "table")
  expect_s3_class(result_table, "data.frame")
  
  # Test plot return (default)
  result_plot <- sq_data %>%
    create_bar(hrvar = "Organization", metric = "Email_hours", return = "plot")
  expect_s3_class(result_plot, "ggplot")
})

test_that("create_bar handles missing variables gracefully", {
  expect_error(
    sq_data %>% create_bar(hrvar = "NonexistentVar", metric = "Email_hours")
  )
  expect_error(
    sq_data %>% create_bar(hrvar = "Organization", metric = "NonexistentMetric")
  )
})
