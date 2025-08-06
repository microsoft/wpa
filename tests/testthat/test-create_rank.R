test_that("create_rank produces ggplot object", {
  result <- sq_data %>%
    create_rank(hrvar = "Organization", metric = "Email_hours")
  expect_s3_class(result, "ggplot")
})

test_that("create_rank handles different return types", {
  # Test table return
  result_table <- sq_data %>%
    create_rank(hrvar = "Organization", metric = "Email_hours", return = "table")
  expect_s3_class(result_table, "data.frame")
  
  # Test plot return
  result_plot <- sq_data %>%
    create_rank(hrvar = "Organization", metric = "Email_hours", return = "plot")
  expect_s3_class(result_plot, "ggplot")
})

test_that("create_rank with different modes", {
  result <- sq_data %>%
    create_rank(hrvar = "Organization", metric = "Email_hours", mode = "simple")
  expect_s3_class(result, "ggplot")
})
