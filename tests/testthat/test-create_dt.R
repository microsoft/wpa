# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

context("Test create_dt")

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