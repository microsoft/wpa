# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

context("Test hrvar_count_all")

test_that("hrvar_count_all correctly identifies text missing values", {
  # Create a test dataframe with NA and text missing values
  test_data <- data.frame(
    PersonId = c(1, 2, 3, 4, 5, 6),
    Organization = c("Org1", "Org2", NA, "N/A", " ", "#N/A"),
    LevelDesignation = c("L1", "L2", "L3", "NA", "L5", "L6"),
    FunctionType = c("F1", "F2", "F3", "F4", "F5", "F6"),
    stringsAsFactors = FALSE
  )
  
  # Run with default na_values
  result <- hrvar_count_all(test_data, return = "text", maxna = 50)
  
  # Check if the message mentions potential missing values
  expect_true(grepl("potentially represent missing values", result))
  
  # Run with custom na_values
  result_custom <- hrvar_count_all(
    test_data, 
    return = "text", 
    maxna = 50,
    na_values = c("N/A")
  )
  
  # Check if the message mentions N/A specifically
  expect_true(grepl("N/A", result_custom))
  
  # Run with empty na_values
  result_none <- hrvar_count_all(
    test_data, 
    return = "text", 
    maxna = 50,
    na_values = c()
  )
  
  # Should not mention potential missing values for text strings
  expect_false(grepl("potentially represent missing values", result_none))
})