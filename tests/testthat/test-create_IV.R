# Test for create_IV function
test_that(
  desc = "create_IV accepts logical outcome variables",
  code = {
    # Create a logical outcome variable
    test_data <- sq_data %>%
      dplyr::mutate(outcome_logical = Workweek_span > 40)
    
    # Test with logical outcome - should not error
    expect_error(
      create_IV(
        data = test_data,
        outcome = "outcome_logical",
        predictors = c("Email_hours", "Meeting_hours"),
        return = "summary"
      ),
      NA
    )
    
    # Verify the output is a data frame when using logical outcome
    result <- create_IV(
      data = test_data,
      outcome = "outcome_logical",
      predictors = c("Email_hours", "Meeting_hours"),
      return = "summary"
    )
    expect_s3_class(result, "data.frame")
    
    # Verify the function handles both logical and numeric binary variables
    # First with numeric
    test_data_numeric <- sq_data %>%
      dplyr::mutate(outcome_numeric = as.numeric(Workweek_span > 40))
    
    result_numeric <- create_IV(
      data = test_data_numeric,
      outcome = "outcome_numeric",
      predictors = c("Email_hours", "Meeting_hours"),
      return = "summary"
    )
    
    # Then with logical
    result_logical <- create_IV(
      data = test_data,
      outcome = "outcome_logical",
      predictors = c("Email_hours", "Meeting_hours"),
      return = "summary"
    )
    
    # The results should be the same regardless of input type
    expect_equal(
      result_numeric$IV,
      result_logical$IV
    )
  }
)

# Test for create_IV function with categorical predictors
test_that(
  desc = "create_IV accepts categorical predictors",
  code = {
    # Create a binary outcome variable
    test_data <- sq_data %>%
      dplyr::mutate(outcome_binary = as.numeric(Workweek_span > 40))
    
    # Test with categorical predictor - should not error
    expect_error(
      create_IV(
        data = test_data,
        outcome = "outcome_binary",
        predictors = c("FunctionType", "Email_hours"),
        return = "summary"
      ),
      NA
    )
    
    # Verify the output is a data frame when using categorical predictors
    result <- create_IV(
      data = test_data,
      outcome = "outcome_binary",
      predictors = c("FunctionType", "Email_hours"),
      return = "summary"
    )
    expect_s3_class(result, "data.frame")
    
    # Verify that both categorical and numeric predictors appear in results
    expect_true("FunctionType" %in% result$Variable)
    expect_true("Email_hours" %in% result$Variable)
    
    # Test with only categorical predictor
    result_cat_only <- create_IV(
      data = test_data,
      outcome = "outcome_binary",
      predictors = "FunctionType",
      return = "summary"
    )
    expect_s3_class(result_cat_only, "data.frame")
    expect_equal(nrow(result_cat_only), 1)
    expect_equal(result_cat_only$Variable[1], "FunctionType")
    
    # Test that categorical predictors are automatically included when predictors=NULL
    result_auto <- create_IV(
      data = test_data,
      outcome = "outcome_binary",
      predictors = NULL,
      return = "summary"
    )
    expect_s3_class(result_auto, "data.frame")
    # Check that some categorical variables are included in the automatic selection
    categorical_vars_in_data <- names(test_data)[sapply(test_data, function(x) is.character(x) || is.factor(x))]
    categorical_vars_in_result <- intersect(categorical_vars_in_data, result_auto$Variable)
    expect_true(length(categorical_vars_in_result) > 0)
  }
)