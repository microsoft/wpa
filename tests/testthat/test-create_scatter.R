test_that("create_scatter basic functionality", {
  # Test basic plot return
  expect_error(
    sq_data %>% create_scatter(hrvar = "Organization", 
                               metric_x = "Email_hours", 
                               metric_y = "Meeting_hours"),
    NA
  )
  
  result <- sq_data %>%
    create_scatter(hrvar = "Organization", 
                   metric_x = "Email_hours", 
                   metric_y = "Meeting_hours")
  expect_s3_class(result, "ggplot")
  
  # Test table return
  result_table <- sq_data %>%
    create_scatter(hrvar = "Organization", 
                   metric_x = "Email_hours", 
                   metric_y = "Meeting_hours",
                   return = "table")
  expect_s3_class(result_table, "data.frame")
})
