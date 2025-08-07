# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

test_that("create_dt show_rows parameter works correctly", {
  # Create a simple test dataframe
  test_data <- data.frame(
    PersonId = 1:100,
    Organization = paste0("Org", 1:100),
    Value = rnorm(100),
    stringsAsFactors = FALSE
  )
  
  # Test default behavior (show_rows = 10)
  dt_default <- create_dt(test_data)
  expect_s3_class(dt_default, "datatables")
  
  # Test show_rows = 25
  dt_25 <- create_dt(test_data, show_rows = 25)
  expect_s3_class(dt_25, "datatables")
  
  # Test show_rows = "All"
  dt_all <- create_dt(test_data, show_rows = "All")
  expect_s3_class(dt_all, "datatables")
  
  # Test show_rows = -1 (equivalent to "All")
  dt_all_numeric <- create_dt(test_data, show_rows = -1)
  expect_s3_class(dt_all_numeric, "datatables")
})

test_that("create_dt show_rows parameter constructs correct lengthMenu", {
  # Create a simple test dataframe
  test_data <- data.frame(
    A = 1:5,
    B = letters[1:5],
    stringsAsFactors = FALSE
  )
  
  # Test that function runs without error for different show_rows values
  expect_no_error(create_dt(test_data, show_rows = 10))
  expect_no_error(create_dt(test_data, show_rows = 25))
  expect_no_error(create_dt(test_data, show_rows = 50))
  expect_no_error(create_dt(test_data, show_rows = "All"))
  expect_no_error(create_dt(test_data, show_rows = -1))
})

test_that("create_dt rounding parameter works with numeric values (backward compatibility)", {
  # Create test data with numeric columns
  test_data <- data.frame(
    Name = c("A", "B", "C"),
    Value1 = c(1.23456, 2.78901, 3.45678),
    Value2 = c(0.1234, 0.5678, 0.9012),
    Count = c(10L, 20L, 30L),
    stringsAsFactors = FALSE
  )
  
  # Test with numeric rounding values
  expect_no_error(create_dt(test_data, rounding = 1))
  expect_no_error(create_dt(test_data, rounding = 2))
  expect_no_error(create_dt(test_data, rounding = 0))
  
  # Should return datatables object
  result <- create_dt(test_data, rounding = 2)
  expect_s3_class(result, "datatables")
})

test_that("create_dt rounding parameter works with named lists", {
  # Create test data with numeric columns
  test_data <- data.frame(
    Name = c("A", "B", "C"),
    Value1 = c(1.23456, 2.78901, 3.45678),
    Value2 = c(0.1234, 0.5678, 0.9012),
    Count = c(10L, 20L, 30L),
    stringsAsFactors = FALSE
  )
  
  # Test with list rounding
  expect_no_error(create_dt(test_data, rounding = list("Value1" = 1, "Value2" = 3)))
  expect_no_error(create_dt(test_data, rounding = list("Count" = 0)))
  expect_no_error(create_dt(test_data, rounding = list("Value1" = 2, "Value2" = 1, "Count" = 0)))
  
  # Should return datatables object
  result <- create_dt(test_data, rounding = list("Value1" = 1, "Value2" = 3))
  expect_s3_class(result, "datatables")
})

test_that("create_dt handles edge cases for rounding parameter", {
  # Test data with only non-numeric columns
  non_numeric_data <- data.frame(
    Name = c("A", "B", "C"),
    Category = c("X", "Y", "Z"),
    stringsAsFactors = FALSE
  )
  
  # Should work with non-numeric data regardless of rounding parameter
  expect_no_error(create_dt(non_numeric_data, rounding = 2))
  expect_no_error(create_dt(non_numeric_data, rounding = list("Name" = 1)))
  
  # Test data with numeric columns
  numeric_data <- data.frame(
    Value1 = c(1.23, 2.34),
    Value2 = c(3.45, 4.56)
  )
  
  # Test with empty list
  expect_no_error(create_dt(numeric_data, rounding = list()))
  
  # Test with list containing non-existent column names
  expect_no_error(create_dt(numeric_data, rounding = list("NonExistent" = 2, "Value1" = 1)))
  
  # Should still return datatables object
  result <- create_dt(numeric_data, rounding = list("NonExistent" = 2, "Value1" = 1))
  expect_s3_class(result, "datatables")
})

test_that("create_dt rounding works with percentage formatting", {
  # Create test data
  test_data <- data.frame(
    Category = c("A", "B"),
    Rate1 = c(0.234, 0.567),
    Rate2 = c(0.123, 0.789),
    stringsAsFactors = FALSE
  )
  
  # Test numeric rounding with percentages
  expect_no_error(create_dt(test_data, rounding = 2, percent = TRUE))
  
  # Test list rounding with percentages
  expect_no_error(create_dt(test_data, rounding = list("Rate1" = 1, "Rate2" = 3), percent = TRUE))
  
  # Should return datatables object
  result <- create_dt(test_data, rounding = list("Rate1" = 1, "Rate2" = 3), percent = TRUE)
  expect_s3_class(result, "datatables")
})

test_that("create_dt preserves all other functionality with new rounding feature", {
  # Create test data
  test_data <- data.frame(
    Name = paste0("Item", 1:20),
    Value1 = runif(20, 1, 10),
    Value2 = runif(20, 0, 1),
    stringsAsFactors = FALSE
  )
  
  # Test that all combinations work
  expect_no_error(create_dt(test_data, 
                           rounding = list("Value1" = 2, "Value2" = 3), 
                           freeze = 1, 
                           percent = FALSE, 
                           show_rows = 15))
  
  expect_no_error(create_dt(test_data, 
                           rounding = list("Value2" = 4), 
                           freeze = 2, 
                           percent = TRUE, 
                           show_rows = "All"))
  
  # All should return datatables objects
  result1 <- create_dt(test_data, rounding = list("Value1" = 2), freeze = 1)
  result2 <- create_dt(test_data, rounding = 2, freeze = 1)  # backward compatibility
  
  expect_s3_class(result1, "datatables")
  expect_s3_class(result2, "datatables")
})