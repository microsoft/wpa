# Test for create_IV function

# Setup shared test data
setup_test_data <- function() {
  wpa::sq_data %>%
    dplyr::mutate(
      outcome_logical = Workweek_span > 40,
      outcome_numeric = as.numeric(Workweek_span > 40)
    )
}

test_that("create_IV handles different outcome variable types", {
  test_data <- setup_test_data()
  
  # Test with logical outcome
  result_logical <- create_IV(
    data = test_data,
    outcome = "outcome_logical",
    predictors = c("Email_hours", "Meeting_hours"),
    return = "summary"
  )
  expect_s3_class(result_logical, "data.frame")
  
  # Test with numeric binary outcome  
  result_numeric <- create_IV(
    data = test_data,
    outcome = "outcome_numeric", 
    predictors = c("Email_hours", "Meeting_hours"),
    return = "summary"
  )
  expect_s3_class(result_numeric, "data.frame")
  
  # Results should be equivalent regardless of input type
  expect_equal(result_numeric$IV, result_logical$IV)
})

test_that("create_IV handles different predictor types", {
  test_data <- setup_test_data()
  
  # Test with mixed predictors (categorical + numeric)
  result_mixed <- create_IV(
    data = test_data,
    outcome = "outcome_numeric",
    predictors = c("FunctionType", "Email_hours"),
    return = "summary"
  )
  expect_s3_class(result_mixed, "data.frame")
  expect_true("FunctionType" %in% result_mixed$Variable)
  expect_true("Email_hours" %in% result_mixed$Variable)
  
  # Test with categorical only
  result_categorical <- create_IV(
    data = test_data,
    outcome = "outcome_numeric",
    predictors = "FunctionType",
    return = "summary"
  )
  expect_s3_class(result_categorical, "data.frame")
  expect_equal(nrow(result_categorical), 1)
  expect_equal(result_categorical$Variable[1], "FunctionType")
  
  # Test automatic predictor selection (NULL)
  result_auto <- create_IV(
    data = test_data,
    outcome = "outcome_numeric",
    predictors = NULL,
    return = "summary"
  )
  expect_s3_class(result_auto, "data.frame")
  expect_true(nrow(result_auto) > 0)
})

test_that("create_IV return parameter works correctly", {
  test_data <- setup_test_data()
  
  # Test summary return (default)
  result_summary <- create_IV(
    data = test_data,
    outcome = "outcome_numeric",
    predictors = c("Email_hours", "Meeting_hours"),
    return = "summary"
  )
  expect_s3_class(result_summary, "data.frame")
  expect_true(all(c("Variable", "IV") %in% names(result_summary)))
  
  # Test plot return
  result_plot <- create_IV(
    data = test_data,
    outcome = "outcome_numeric", 
    predictors = c("Email_hours", "Meeting_hours"),
    return = "plot"
  )
  expect_s3_class(result_plot, "ggplot")
  
  # Test IV return
  result_iv <- create_IV(
    data = test_data,
    outcome = "outcome_numeric",
    predictors = c("Email_hours", "Meeting_hours"), 
    return = "IV"
  )
  expect_type(result_iv, "list")
})

test_that("create_IV handles edge cases and errors", {
  test_data <- setup_test_data()
  
  # Test with non-existent outcome
  expect_error(
    create_IV(
      data = test_data,
      outcome = "nonexistent_outcome",
      predictors = c("Email_hours"),
      return = "summary"
    )
  )
  
  # Test with non-existent predictor
  expect_error(
    create_IV(
      data = test_data,
      outcome = "outcome_numeric",
      predictors = "nonexistent_predictor", 
      return = "summary"
    )
  )
  
  # Test with non-binary outcome
  expect_error(
    create_IV(
      data = test_data,
      outcome = "Email_hours",  # continuous variable
      predictors = c("Meeting_hours"),
      return = "summary"
    )
  )
})

test_that("create_IV parameters work correctly", {
  test_data <- setup_test_data()
  
  # Test different bin values
  expect_no_error(
    create_IV(
      data = test_data,
      outcome = "outcome_numeric",
      predictors = "Email_hours",
      bins = 3,
      return = "summary"
    )
  )
  
  # Test significance level parameter
  expect_no_error(
    create_IV(
      data = test_data,
      outcome = "outcome_numeric", 
      predictors = "Email_hours",
      siglevel = 0.01,
      return = "summary"
    )
  )
  
  # Test exclude significance parameter
  expect_no_error(
    create_IV(
      data = test_data,
      outcome = "outcome_numeric",
      predictors = "Email_hours", 
      exc_sig = TRUE,
      return = "summary"
    )
  )
})