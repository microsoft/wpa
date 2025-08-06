test_that("create_scatter produces ggplot object", {
  result <- sq_data %>%
    create_scatter(hrvar = "Organization", 
                   metric_x = "Email_hours", 
                   metric_y = "Meeting_hours")
  expect_s3_class(result, "ggplot")
})

test_that("create_scatter handles different return types", {
  # Test table return
  result_table <- sq_data %>%
    create_scatter(hrvar = "Organization", 
                   metric_x = "Email_hours", 
                   metric_y = "Meeting_hours",
                   return = "table")
  expect_s3_class(result_table, "data.frame")
})

test_that("create_scatter error handling", {
  expect_error(
    sq_data %>% 
    create_scatter(hrvar = "Organization", 
                   metric_x = "InvalidMetric", 
                   metric_y = "Meeting_hours")
  )
})
