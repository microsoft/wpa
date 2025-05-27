# Load required libraries and source functions
library(dplyr)
library(magrittr)

# Source the create_IV function and its dependencies
source("/home/runner/work/wpa/wpa/R/create_IV.R")
# Try to source other necessary functions
tryCatch({
  source("/home/runner/work/wpa/wpa/R/create_bar.R")
  source("/home/runner/work/wpa/wpa/R/map_IV.R")
  source("/home/runner/work/wpa/wpa/R/p_test.R")
}, error = function(e) {
  message(paste("Warning: Could not source a dependency:", e$message))
})

# Print test message
message("Testing create_IV with logical outcome...")

# Create test data
load("/home/runner/work/wpa/wpa/data/sq_data.rda")
test_data <- sq_data
test_data$outcome_logical <- test_data$Workweek_span > 40

# Try to run create_IV with a logical outcome
tryCatch({
  result <- create_IV(
    data = test_data,
    outcome = "outcome_logical",
    predictors = c("Email_hours", "Meeting_hours"),
    return = "summary"
  )
  message("Test passed: create_IV accepts logical outcome variables.")
  message("Result structure:")
  str(result)
}, error = function(e) {
  message(paste("Test failed with error:", e$message))
})