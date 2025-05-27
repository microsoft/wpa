test_that("create_IV handles logical outcome without error", {
  df <- sq_data %>%
    dplyr::mutate(outcome = Email_hours > 10)
  expect_error(
    create_IV(df, predictors = c("Meeting_hours", "Collaboration_hours"), outcome = "outcome", return = "summary"),
    NA
  )
})
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