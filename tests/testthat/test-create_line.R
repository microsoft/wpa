test_that("create_line basic functionality", {
  # Test basic plot return
  expect_error(
    sq_data %>% create_line(hrvar = "Organization", metric = "Email_hours"),
    NA
  )
  
  result <- sq_data %>%
    create_line(hrvar = "Organization", metric = "Email_hours")
  expect_s3_class(result, "ggplot")
  
  # Test table return
  result_table <- sq_data %>%
    create_line(hrvar = "Organization", metric = "Email_hours", return = "table")
  expect_s3_class(result_table, "data.frame")
})
