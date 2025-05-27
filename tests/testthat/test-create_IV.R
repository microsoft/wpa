test_that("create_IV handles logical outcome without error", {
  df <- sq_data %>%
    dplyr::mutate(outcome = Email_hours > 10)
  expect_error(
    create_IV(df, predictors = c("Meeting_hours", "Collaboration_hours"), outcome = "outcome", return = "summary"),
    NA
  )
})
